use crate::monomorphize;
use crate::souce_builder::{self, CSourceBuilder};
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

pub fn rust_shim<'tcx>(
    souce_builder: &mut CSourceBuilder<'tcx>,
    original_name: &str,
    shim_name: &str,
    instance: Instance<'tcx>,
    tcx: TyCtxt<'tcx>,
) {
    //
    let uncodumented = rustc_middle::ty::List::empty();
    let abi = tcx
        .fn_abi_of_instance(PseudoCanonicalInput {
            typing_env: TypingEnv::fully_monomorphized(),
            value: (instance, uncodumented),
        })
        .expect("Could not compute fn abi");
    let args = crate::function::arg_names(instance, tcx, abi.args.len());
    let shim_ret: String = rust_type_string(abi.ret.layout.ty, tcx, souce_builder, instance, false);
    let mut shim_args: String = (&abi.args)
        .into_iter()
        .zip(args.iter())
        .filter_map(|(arg, name)| {
            // Refence: https://doc.rust-lang.org/stable/nightly-rustc/rustc_target/abi/call/enum.PassMode.html
            match &arg.mode {
                // Ignored, so not in the sig.
                PassMode::Ignore => None,
                // PassMode::Direct:Passed directly by value. MUST be a scalar(initger, char, bool, float) or vector of scalars.
                // Some of the ArgAttibutes is ignored for now, since it *should* be already handled by the C compiler.
                _ => Some(format!(
                    "{}:{}",
                    name,
                    rust_type_string(arg.layout.ty, tcx, souce_builder, instance, false),
                )),
            }
        })
        .intersperse(",".to_string())
        .collect();
    let escaped_shim_name = shim_name.replace('$', "ds");
    let escaped_real_name = original_name.replace('$', "ds");
    souce_builder.add_rust(&format!(
        "extern \"Rust\"{{#[link_name = \"{original_name}\"]pub fn {escaped_real_name}({shim_args})->{shim_ret};}}\n",
    ));
    let mut real_args: String = (&abi.args)
        .into_iter()
        .zip(args.iter())
        .map(|(arg, name)| {
            format!(
                "{}:{}",
                name,
                rust_type_string(arg.layout.ty, tcx, souce_builder, instance, true),
            )
        })
        .intersperse(",".to_string())
        .collect();

    let real_ret: String = rust_type_string(abi.ret.layout.ty, tcx, souce_builder, instance, true);
    let translated_args: String = (&abi.args)
        .into_iter()
        .zip(args.iter())
        .map(|(arg, name)| {
            // Refence: https://doc.rust-lang.org/stable/nightly-rustc/rustc_target/abi/call/enum.PassMode.html
            match &arg.mode {
                // Ignored, so not in the sig.
                PassMode::Ignore => "()".into(),
                // PassMode::Direct:Passed directly by value. MUST be a scalar(initger, char, bool, float) or vector of scalars.
                // Some of the ArgAttibutes is ignored for now, since it *should* be already handled by the C compiler.
                _ => format!("{name}.into()"),
            }
        })
        .intersperse((&",").to_string())
        .collect();
    let translator_body = format!("unsafe{{{escaped_real_name}({translated_args}).into()}}");

    souce_builder.add_rust(&format!(
        "#[export_name = \"{shim_name}\"]\n#[linkage = \"linkonce_odr\"]\npub extern \"C\" fn {escaped_shim_name}({real_args})->{real_ret}{{\n{translator_body}\n}}\n",
    ));
}

pub fn rust_type_string<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    source_builder: &mut crate::souce_builder::CSourceBuilder<'tcx>,
    instance: Instance<'tcx>,
    c_safe: bool,
) -> String {
    let ty = monomorphize(instance, ty, tcx);
    match ty.kind() {
        TyKind::Int(_) | TyKind::Uint(_) | TyKind::Float(_) | TyKind::Bool => format!("{ty:?}"),
        TyKind::Char => "u32".into(),
        TyKind::Ref(_, inner, mutability) | TyKind::RawPtr(inner, mutability) => {
            let mutability = match mutability {
                Mutability::Not => "const",
                Mutability::Mut => "mut",
            };
            if crate::is_fat_ptr(ty, tcx, instance) {
                match inner.kind() {
                    TyKind::Str => {
                        if c_safe {
                            "RustStr".into()
                        } else {
                            "*const str".into()
                        }
                    }
                    TyKind::Slice(elem) => {
                        if c_safe {
                            "RustSlice".into()
                        } else {
                            format!(
                                "*const [{elem}]",
                                elem =
                                    rust_type_string(*elem, tcx, source_builder, instance, c_safe)
                            )
                        }
                    }
                    TyKind::Dynamic(_, _, _) => {
                        if c_safe {
                            "RustDyn".into()
                        } else {
                            "*const [u8]".into()
                        }
                    }
                    _ => format!("RustFatPtr",),
                }
            } else if souce_builder::is_zst(*inner, tcx) {
                format!("*{mutability} ()")
            } else {
                format!(
                    "*{mutability} {}",
                    rust_type_string(*inner, tcx, source_builder, instance, c_safe)
                )
            }
        }
        TyKind::Adt(def, gargs) => {
            let adt_instance =
                Instance::try_resolve(tcx, TypingEnv::fully_monomorphized(), def.did(), gargs)
                    .unwrap()
                    .unwrap();
            // Get the mangled path: it is absolute, and not poluted by types being rexported
            crate::instance_ident(adt_instance, tcx).replace('$', "ds")
        }
        TyKind::Closure(did, gargs) => {
            let adt_instance =
                Instance::try_resolve(tcx, TypingEnv::fully_monomorphized(), *did, gargs)
                    .unwrap()
                    .unwrap();
            // Get the mangled path: it is absolute, and not poluted by types being rexported
            crate::instance_ident(adt_instance, tcx).replace('$', "ds")
        }
        TyKind::Tuple(elements) => {
            if elements.is_empty() {
                "()".into()
            } else if c_safe {
                format!("{}", crate::function::mangle(ty, tcx))
            } else {
                format!(
                    "({})",
                    elements
                        .iter()
                        .map(|ty| rust_type_string(ty, tcx, source_builder, instance, c_safe))
                        .intersperse(",".to_string())
                        .collect::<String>()
                )
            }
        }
        TyKind::Slice(_) => "RawSlice".into(),
        TyKind::Array(elem, length) => format!(
            "[{elem};{length}]",
            elem = rust_type_string(*elem, tcx, source_builder, instance, c_safe)
        ),
        TyKind::Never => {
            if c_safe {
                eprintln!("Never in C.");
                "!".into()
            } else {
                "!".into()
            }
        }
        TyKind::FnDef(_, _) => {
            if c_safe {
                eprintln!("FnDef in C.");
                "()".into()
            } else {
                "()".into()
            }
        }
        TyKind::FnPtr(_, _) => "*const ()".to_owned(),
        TyKind::Dynamic(_, _, _) => "Dynamic".into(),
        _ => todo!("Can't convert {ty:?} to a Rust type."),
    }
}
