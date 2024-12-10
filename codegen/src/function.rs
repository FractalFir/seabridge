use rustc_middle::mir::mono::MonoItemData;
use rustc_middle::ty::FloatTy;
use rustc_middle::ty::Instance;
use rustc_middle::ty::IntTy;
use rustc_middle::ty::PseudoCanonicalInput;
use rustc_middle::ty::Ty;
use rustc_middle::ty::TyCtxt;
use rustc_middle::ty::TyKind;
use rustc_middle::ty::TypingEnv;
use rustc_middle::ty::UintTy;

use rustc_target::abi::call::ArgAttribute;
use rustc_target::abi::Reg;
use rustc_target::abi::RegKind;
use rustc_target::abi::Size;
use rustc_target::abi::Variants;
use rustc_target::callconv::CastTarget;
use rustc_target::callconv::PassMode;

use rustc_hir::Mutability;

use crate::monomorphize;
use crate::souce_builder::is_zst;
use crate::souce_builder::CSourceBuilder;

use std::fmt::Write;
/// Compiles a function into a C function defintion.
pub(crate) fn compile_function<'tcx>(
    finstance: Instance<'tcx>,
    _data: MonoItemData,
    source_builder: &mut crate::souce_builder::CSourceBuilder<'tcx>,
    tcx: TyCtxt<'tcx>,
) {
    if source_builder.is_defined(finstance) {
        return;
    }
    let visibility = tcx.visibility(finstance.def_id());
    if !visibility.is_public() {
        return;
    }
    source_builder.add_fn_def(finstance, tcx);
}
/// Retrives the arg names from debug info when possible, otherwise retunring a set of unqiue names otherwise.
pub fn arg_names<'tcx>(_instance: Instance<'tcx>, _tcx: TyCtxt<'tcx>, args: usize) -> Vec<String> {
    (0..args).map(|arg| format!("a{arg}")).collect()
}
/// Turns the `CastTarget` into a list of variables, that correspond that all of the registers. If `arg_name` is Some, this is sutiable to be inserted into a function argument list.
/// If not, this is suitable to be the body of a struct(used in returns)
fn pass_mode_cast_elems(pad_i32: bool, cast: &CastTarget, arg_name: Option<&str>) -> String {
    // If the name is_some, preformat it. If no, just return an empty string.
    let name = arg_name.map_or(String::new(), |name| format!("_{name}"));
    // This Vec contains all the fields / args `CastTarget` is turned into.
    let mut elems = vec![];
    // If a padding register is requesred, add it.
    if pad_i32 {
        elems.push(format!("int32_t dummy{name},"));
    };
    // Handle the prefix (a set of identical registers)
    elems.extend(
        cast.prefix
            .iter()
            .flatten()
            .enumerate()
            .map(|(id, reg)| format!("{} prefix_{id}{name}", reg_to_type(*reg))),
    );
    // TODO: don't ignore `is_consecuitve`, and figure out what it is supposed to be.
    // Divide the total size passed in `rest` by the size of an individual register, to get the ammount of registers.
    // Round down, since the last register can have a smaller size.
    let rest_count = cast
        .rest
        .total
        .bytes()
        .div_floor(cast.rest.unit.size.bytes());
    // Emmit the main, identicaly-sized registers.
    elems.extend(
        (0..rest_count)
            .map(|id| format!("{} rest_{id}{name}", reg_to_type(cast.rest.unit)))
            .intersperse(",".into()),
    );
    // If the total size is not a mutiple of the register size, the last register may be smaller.
    let reminder = cast.rest.total.bytes() % cast.rest.unit.size.bytes();
    if reminder != 0 {
        elems.push(format!(
            "{} rest_{rest_count}{name}",
            reg_to_type_inner(cast.rest.unit.kind, Size::from_bytes(reminder))
        ));
    }
    // Collect all the elements, and separate them correctly.
    elems.into_iter().intersperse(",".to_string()).collect()
}
pub fn call_shim<'tcx>(
    instance: Instance<'tcx>,
    tcx: TyCtxt<'tcx>,
    shim_name: &str,
    source_builder: &mut CSourceBuilder,
) -> String {
    let uncodumented = rustc_middle::ty::List::empty();

    let abi = tcx
        .fn_abi_of_instance(PseudoCanonicalInput {
            typing_env: TypingEnv::fully_monomorphized(),
            value: (instance, uncodumented),
        })
        .expect("Could not compute fn abi");

    let args = arg_names(instance, tcx, abi.args.len());
    let args: String = (&abi.args)
        .into_iter()
        .zip(args.iter())
        .filter_map(|(arg, name)| {
            // Refence: https://doc.rust-lang.org/stable/nightly-rustc/rustc_target/abi/call/enum.PassMode.html
            match &arg.mode {
                // Ignored, so not in the sig.
                PassMode::Ignore => None,

                _ => Some(name.as_ref()),
            }
        })
        .intersperse(",")
        .collect();
    match &abi.ret.mode {
        PassMode::Ignore => {
            format!("\t{shim_name}({args});\n")
        }
        _ => {
            format!("\treturn {shim_name}({args});\n")
        }
    }
}
/// Creates the declaration of this funcion(its signature and name).
#[allow(clippy::format_collect, clippy::too_many_lines)]
pub fn fn_decl<'tcx>(
    instance: Instance<'tcx>,
    tcx: TyCtxt<'tcx>,
    source_builder: &mut crate::souce_builder::CSourceBuilder<'tcx>,
    fn_name: &str,
) -> String {
    // The purpose of this arg is not documented...
    let uncodumented = rustc_middle::ty::List::empty();

    let abi = tcx
        .fn_abi_of_instance(PseudoCanonicalInput {
            typing_env: TypingEnv::fully_monomorphized(),
            value: (instance, uncodumented),
        })
        .expect("Could not compute fn abi");
    // Handle the ABI of all the argument types
    let args = arg_names(instance, tcx, abi.args.len());
    let mut args: String = (&abi.args)
        .into_iter()
        .zip(args.iter())
        .filter_map(|(arg, name)| {
            // Refence: https://doc.rust-lang.org/stable/nightly-rustc/rustc_target/abi/call/enum.PassMode.html
            match &arg.mode {
                // Ignored, so not in the sig.
                PassMode::Ignore => None,
                // PassMode::Direct:Passed directly by value. MUST be a scalar(initger, char, bool, float) or vector of scalars.
                // Some of the ArgAttibutes is ignored for now, since it *should* be already handled by the C compiler.
                PassMode::Direct(attrs) => {
                    let restrict = if attrs.regular.contains(ArgAttribute::NoAlias)
                        && source_builder.supports_restrict()
                        && arg.layout.ty.is_primitive()
                    {
                        " restrict"
                    } else {
                        ""
                    };
                    Some(format!(
                        "{}{restrict} {name}",
                        c_type_string(arg.layout.ty, tcx, source_builder, instance)
                    ))
                }

                _ => Some(format!(
                    "{} {}",
                    c_type_string(arg.layout.ty, tcx, source_builder, instance),
                    name
                )),
            }
        })
        .intersperse(",".to_string())
        .collect();
    let ret: String = match &abi.ret.mode {
        PassMode::Ignore => "void".into(),

        // PassMode::Direct:Passed directly by value. MUST be a scalar(initger, char, bool, float) or vector of scalars.
        // Some of the ArgAttibutes is ignored for now, since it *should* be already handled by the C compiler.
        PassMode::Direct(attrs) => {
            let restrict = if attrs.regular.contains(ArgAttribute::NoAlias)
                && source_builder.supports_restrict()
            {
                "restrict "
            } else {
                ""
            };
            format!(
                "{restrict}{}",
                c_type_string(abi.ret.layout.ty, tcx, source_builder, instance)
            )
        }
        _ => {
            (format!(
                "{}",
                c_type_string(abi.ret.layout.ty, tcx, source_builder, instance),
            ))
        }
    };

    format!("{ret} {fn_name}({args})")
}

/// Turns a given type `ty` into a C type string, adding typedefs if need be.
#[allow(clippy::format_collect, clippy::too_many_lines)]
pub fn c_type_string<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    source_builder: &mut crate::souce_builder::CSourceBuilder<'tcx>,
    instance: Instance<'tcx>,
) -> String {
    source_builder.add_ty_templates(ty, tcx);
    if !source_builder.delayed_typedefs().contains(&ty) {
        source_builder.add_typedefs(ty, tcx, instance);
    }

    let ty = monomorphize(instance, ty, tcx);
    match ty.kind() {
        TyKind::Array(elem, length) => format!(
            "struct RustArr<{elem},{length}>",
            elem = c_type_string(*elem, tcx, source_builder, instance)
        ),
        TyKind::RawPtr(inner, mutability) | TyKind::Ref(_, inner, mutability) => {
            let mutability = match mutability {
                Mutability::Not => "const",
                Mutability::Mut => "",
            };
            if crate::is_fat_ptr(ty, tcx, instance) {
                match inner.kind() {
                    TyKind::Str => {
                        format!("RustStr")
                    }
                    TyKind::Slice(elem) => {
                        let tpe = c_type_string(*elem, tcx, source_builder, instance);
                        format!("RustSlice<{tpe}>")
                    }
                    TyKind::Dynamic(_, _, _) => format!("RustDyn"),
                    _ => format!(
                        "RustFatPtr<{inner}>",
                        inner = c_type_string(*inner, tcx, source_builder, instance)
                    ),
                }
            } else if is_zst(*inner, tcx) {
                format!("void {mutability}*")
            } else {
                format!(
                    "{} {mutability}*",
                    c_type_string(*inner, tcx, source_builder, instance)
                )
            }
        } /*
        TyKind::Ref(_, inner, mutability) => {
        let mutability = match mutability {
        Mutability::Not => "const",
        Mutability::Mut => "",
        };
        if crate::is_fat_ptr(ty, tcx, instance) {
        match inner.kind() {
        TyKind::Str => {
        format!("RustStr")
        }
        TyKind::Slice(elem) => {
        let tpe = c_type_string(*elem, tcx, source_builder, instance);
        format!("RustSlice<{tpe}>")
        }
        TyKind::Dynamic(_, _, _) => format!("RustDyn"),
        _ => format!(
        "RustFatPtr<{inner}>",
        inner = c_type_string(*inner, tcx, source_builder, instance)
        ),
        }
        } else if is_zst(*inner, tcx) {
        format!("void {mutability}*")
        } else {
        format!(
        "{} {mutability}&",
        c_type_string(*inner, tcx, source_builder, instance)
        )
        }
        }*/
        TyKind::Char => "uint32_t".into(),
        TyKind::Bool => "bool".into(),
        TyKind::Int(int) => match int {
            IntTy::I8 => "int8_t",
            IntTy::I16 => "int16_t",
            IntTy::I32 => "int32_t",
            IntTy::I64 => "int64_t",
            IntTy::I128 => {
                if source_builder.supports_i128() {
                    "__int128_t"
                } else {
                    todo!("Can't yet emulate i128.")
                }
            }
            IntTy::Isize => "intptr_t",
        }
        .into(),
        TyKind::Uint(uint) => match uint {
            UintTy::U8 => "uint8_t",
            UintTy::U16 => "uint16_t",
            UintTy::U32 => "uint32_t",
            UintTy::U64 => "uint64_t",
            UintTy::U128 => {
                if source_builder.supports_i128() {
                    "__uint128_t"
                } else {
                    todo!("Can't yet emulate i128.")
                }
            }
            UintTy::Usize => "uintptr_t",
        }
        .into(),
        TyKind::Float(float) => match float {
            FloatTy::F16 => {
                if source_builder.supports_f16() {
                    "_Float16".into()
                } else {
                    todo!("Can't emulate f16 yet.")
                }
            }
            FloatTy::F32 => "float".into(),
            FloatTy::F64 => "double".into(),
            FloatTy::F128 => {
                if source_builder.supports_f128() {
                    "__float128".into()
                } else {
                    todo!("Can't emulate f128 yet.")
                }
            }
        },
        TyKind::Adt(def, gargs) => {
            let adt_instance =
                Instance::try_resolve(tcx, TypingEnv::fully_monomorphized(), def.did(), gargs)
                    .unwrap()
                    .unwrap();
            // Get the mangled path: it is absolute, and not poluted by types being rexported
            let ident = crate::instance_ident(adt_instance, tcx);
            let generic_string =
                crate::souce_builder::generic_string(gargs, tcx, source_builder, instance);
            if let Some(path) = crate::souce_builder::symbol_to_path(&ident) {
                format!(
                    "{}{generic_string}",
                    path.iter()
                        .map(|s| format!("::{}", s.as_str()))
                        .collect::<String>()
                )
            } else {
                format!("{ident}{generic_string}",)
            }
        }
        TyKind::FnPtr(_, _) => "RustFn*".into(),
        TyKind::Closure(did, gargs) => {
            let adt_instance =
                Instance::try_resolve(tcx, TypingEnv::fully_monomorphized(), *did, gargs)
                    .unwrap()
                    .unwrap();
            // Get the mangled path: it is absolute, and not poluted by types being rexported
            format!("struct {}", crate::instance_ident(adt_instance, tcx))
        }
        TyKind::Tuple(elems) => {
            let generic_string: String = elems
                .iter()
                .map(|ty| {
                    crate::souce_builder::generic_ty_string(ty, tcx, source_builder, instance)
                })
                .intersperse(",".into())
                .collect();
            let generic_string = format!("<{generic_string}>");

            format!("RustTuple{generic_string}")
        }
        TyKind::Slice(_) => mangle(ty, tcx),
        TyKind::Never => "Never".into(),
        TyKind::Str | TyKind::Dynamic(_, _, _) => {
            let name = mangle(ty, tcx);
            format!("struct {name}")
        }
        TyKind::FnDef(_, _) => {
            use std::hash::Hash;
            use std::hash::Hasher;
            use std::hash::SipHasher;

            let mut hasher = SipHasher::new_with_keys(0xDEAD_C0FFE, 0xBEEF_BABE);
            ty.hash(&mut hasher);

            format!("RustFnDef<0x{:x}>", hasher.finish() as u64)
        }
        _ => todo!("Can't turn {ty:?} into a c type", ty = ty.kind()),
    }
}
/// Returns a mangled name of this type.
pub fn mangle<'tcx>(ty: Ty<'tcx>, tcx: TyCtxt<'tcx>) -> String {
    match ty.kind() {
        TyKind::Int(int) => match int {
            IntTy::I8 => "i8",
            IntTy::I16 => "i16",
            IntTy::I32 => "i32",
            IntTy::I64 => "i64",
            IntTy::I128 => "i128",
            IntTy::Isize => "isize",
        }
        .into(),
        TyKind::Uint(int) => match int {
            UintTy::U8 => "u8",
            UintTy::U16 => "u16",
            UintTy::U32 => "u32",
            UintTy::U64 => "u64",
            UintTy::U128 => "u128",
            UintTy::Usize => "usize",
        }
        .into(),
        TyKind::Float(float) => match float {
            FloatTy::F16 => "f16",
            FloatTy::F32 => "f32",
            FloatTy::F64 => "f64",
            FloatTy::F128 => "f128",
        }
        .into(),
        TyKind::Slice(inner) => format!("sl{}", mangle(*inner, tcx)),
        TyKind::Str => "ss".into(),
        TyKind::Array(elem, len) => format!("a{len}{elem}", elem = mangle(*elem, tcx)),
        TyKind::Ref(_, ty, muta) => match muta {
            Mutability::Mut => format!("rm{}", mangle(*ty, tcx)),
            Mutability::Not => format!("rc{}", mangle(*ty, tcx)),
        },
        TyKind::RawPtr(ty, muta) => match muta {
            Mutability::Mut => format!("pm{}", mangle(*ty, tcx)),
            Mutability::Not => format!("pc{}", mangle(*ty, tcx)),
        },
        TyKind::Dynamic(_, _, _) => {
            use std::hash::Hash;
            use std::hash::Hasher;
            let mut s = std::hash::DefaultHasher::new();
            ty.hash(&mut s);
            format!("d{:x}", s.finish())
        }
        TyKind::Adt(def, gargs) => {
            let adt_instance =
                Instance::try_resolve(tcx, TypingEnv::fully_monomorphized(), def.did(), gargs)
                    .unwrap()
                    .unwrap();
            // Get the mangled path: it is absolute, and not poluted by types being rexported
            format!(
                "a{}",
                crate::instance_ident(adt_instance, tcx,).replace('.', "_")
            )
        }
        TyKind::Tuple(elems) => format!(
            "t{}{}",
            elems.len(),
            elems.iter().fold(String::new(), |mut output: String, e| {
                let m = mangle(e, tcx);
                let _ = write!(output, "{m_len}{m}", m_len = m.len());
                output
            })
        ),
        TyKind::Bool => "b".into(),
        TyKind::Char => "c".into(),
        TyKind::FnPtr(binder, _) => {
            let fn_ptr =
                tcx.normalize_erasing_late_bound_regions(TypingEnv::fully_monomorphized(), *binder);
            format!(
                "fp{}{}{}",
                fn_ptr.inputs().len(),
                fn_ptr
                    .inputs()
                    .iter()
                    .fold(String::new(), |mut output: String, e| {
                        let m = mangle(*e, tcx);
                        let _ = write!(output, "{m_len}{m}", m_len = m.len());
                        output
                    }),
                {
                    let m = mangle(fn_ptr.output(), tcx);
                    format!("{m_len}{m}", m_len = m.len())
                }
            )
        }
        TyKind::Closure(did, gargs) => {
            let adt_instance =
                Instance::try_resolve(tcx, TypingEnv::fully_monomorphized(), *did, gargs)
                    .unwrap()
                    .unwrap();
            // Get the mangled path: it is absolute, and not poluted by types being rexported
            format!("{}", crate::instance_ident(adt_instance, tcx))
        }
        _ => todo!("Can't mangle {ty:?}"),
    }
}
/// Turns a Rust register `reg` into a C type. If size is not a power of two, rounds up to `next_power_of_two`
pub fn reg_to_type(reg: Reg) -> String {
    reg_to_type_inner(reg.kind, reg.size)
}
/// Turns a Rust register into a C type. If size is not a power of two, rounds up to `next_power_of_two`
pub fn reg_to_type_inner(reg: RegKind, size: Size) -> String {
    match reg {
        RegKind::Integer => format!("int{size}_t", size = size.bits().next_power_of_two()),
        RegKind::Float => match size.bytes() {
            4 => "float".into(),
            8 => "double".into(),
            _ => todo!("Unknown float of size {} bytes", size.bytes()),
        },
        RegKind::Vector => todo!("Can't handle register of type {reg:?}"),
    }
}
