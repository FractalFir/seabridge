use rustc_middle::mir::mono::Linkage;
use rustc_middle::mir::mono::MonoItemData;
use rustc_middle::ty::FloatTy;
use rustc_middle::ty::Instance;
use rustc_middle::ty::IntTy;
use rustc_middle::ty::TyCtxt;
use rustc_middle::ty::TyKind;
use rustc_middle::ty::UintTy;

use rustc_span::def_id::DefId;

use crate::function::c_type_string;
use crate::souce_builder::is_zst;
/// Turn a Rust static into a C static.
pub(crate) fn static_decl<'tcx>(
    static_def: DefId,
    data: MonoItemData,
    source_bilder: &mut crate::souce_builder::CSourceBuilder<'tcx>,
    tcx: TyCtxt<'tcx>,
) -> Option<String> {
    let inst = Instance::mono(tcx, static_def);
    // Get the linkage of this static - is it common(defined here), extern(defined somehwere else) or static (private in C)
    // This is done first, because in header mode, we don't want to export types used by a private sumbol, which will not be included anyway.
    let attrs = tcx.codegen_fn_attrs(static_def);
    let linkage = match attrs.linkage {
        Some(Linkage::External | Linkage::AvailableExternally) => "extern",
        Some(Linkage::Private | Linkage::Internal) => {
            if source_bilder.header_mode() {
                // If header mode, don't export private symbols.
                return None;
            }
            "staitc"
        }
        Some(Linkage::Common) | None => {
            if source_bilder.header_mode() {
                // If header mode, all statics are extern.
                "extern"
            } else {
                ""
            }
        }
        Some(
            Linkage::LinkOnceAny
            | Linkage::LinkOnceODR
            | Linkage::WeakAny
            | Linkage::Appending
            | Linkage::ExternalWeak
            | Linkage::WeakODR,
        ) => panic!("{:?} unsuported", data.linkage),
    };
    // Get the type of this static, without bound vars(generics), and then turn into a c type.
    let static_ty = tcx.type_of(static_def);
    let static_ty = static_ty.no_bound_vars().expect("Generic static.");
    let tpe = c_type_string(static_ty, tcx, source_bilder, inst);
    // If the static is a ZST, skip it.
    if is_zst(static_ty, tcx) {
        return None;
    }
    // Get the unqiue static name
    let static_name = crate::static_ident(static_def, tcx);
    // If section supported, and specified, provide it
    let section = if source_bilder.supports_section()
        && let Some(section) = attrs.link_section
    {
        format!("__attribute__((section(\"{section:?}\")))")
    } else {
        String::new()
    };
    // Check if this is thread local. If so, mark it as such.
    let thread_local = if attrs
        .flags
        .contains(rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrFlags::THREAD_LOCAL)
    {
        "_ThreadLocal"
    } else {
        ""
    };
    // Assemble a static string
    Some(format!(
        "{thread_local}{section}{linkage} {tpe} {static_name}"
    ))
}
/// Adds a static varaible definiton to this source file.
pub(crate) fn define_static<'tcx>(
    static_def: DefId,
    data: MonoItemData,
    source_bilder: &mut crate::souce_builder::CSourceBuilder<'tcx>,
    tcx: TyCtxt<'tcx>,
) {
    // The linkage, type, and name of the static.
    let Some(decl) = static_decl(static_def, data, source_bilder, tcx) else {
        // If the decl is empty, then this is a private symbol in header mode or a zst, so it should not be exported.
        return;
    };
    // In header mode, only emmit the declaration of the static, but don't initialize it.
    if source_bilder.header_mode() {
        source_bilder.source_file_mut().push(format!("{decl};\n"));
        return;
    }
    let static_def = format!(
        "{decl}{init_static};\n",
        init_static = init_static(static_def, data, source_bilder, tcx)
    );
    source_bilder.source_file_mut().push(static_def);
}
/// Computes the initial value of a static.
pub(crate) fn init_static<'tcx>(
    static_def: DefId,
    data: MonoItemData,
    source_bilder: &mut crate::souce_builder::CSourceBuilder<'tcx>,
    tcx: TyCtxt<'tcx>,
) -> String {
    // Get the type of this static, without bound vars(generics).
    let static_ty = tcx.type_of(static_def);
    let static_ty = static_ty.no_bound_vars().expect("Generic static.");
    // Get the underlying allocation
    let alloc = tcx.eval_static_initializer(static_def).unwrap();
    let alloc_id = tcx.reserve_and_set_memory_alloc(alloc);
    let global_alloc = tcx.global_alloc(alloc_id);
    let attrs = tcx.codegen_fn_attrs(static_def);
    let memory = global_alloc.unwrap_memory();
    let mem_len = memory.0.len();
    let bytes = memory
        .0
        .inspect_with_uninit_and_ptr_outside_interpreter(0..mem_len);
    let val = match static_ty.kind() {
        TyKind::Int(int) => match int {
            IntTy::I8 => format!(
                "((int8_t){:x})",
                i8::from_ne_bytes(bytes.try_into().unwrap())
            ),
            IntTy::I16 => format!(
                "((int16_t){:x})",
                i16::from_ne_bytes(bytes.try_into().unwrap())
            ),
            IntTy::I32 => format!(
                "((int32_t){:x})",
                i32::from_ne_bytes(bytes.try_into().unwrap())
            ),
            IntTy::I64 => format!(
                "((int64_t){:x})",
                i64::from_ne_bytes(bytes.try_into().unwrap())
            ),
            IntTy::Isize => format!(
                "((intptr_t){:x})",
                i64::from_ne_bytes(bytes.try_into().expect("Wrong isize type"))
            ),
            IntTy::I128 => {
                if source_bilder.supports_i128() {
                    let lo = u64::from_ne_bytes(bytes[..8].try_into().unwrap());
                    let hi = u64::from_ne_bytes(bytes[8..].try_into().unwrap());
                    format!("((__int128_t)(((__uint128_t) ({hi:x})) << 64 | ({lo:x})))")
                } else {
                    panic!("emulating 128 int consts not supported ATM.")
                }
            }
        },
        TyKind::Uint(int) => match int {
            UintTy::U8 => format!(
                "((uint8_t){:x})",
                u8::from_ne_bytes(bytes.try_into().unwrap())
            ),
            UintTy::U16 => format!(
                "((uint16_t){:x})",
                u16::from_ne_bytes(bytes.try_into().unwrap())
            ),
            UintTy::U32 => format!(
                "((uint32_t){:x})",
                u32::from_ne_bytes(bytes.try_into().unwrap())
            ),
            UintTy::U64 => format!(
                "((uint64_t){:x})",
                u64::from_ne_bytes(bytes.try_into().unwrap())
            ),
            UintTy::Usize => format!(
                "((uintptr_t){:x})",
                u64::from_ne_bytes(bytes.try_into().expect("Wrong usize type"))
            ),
            UintTy::U128 => {
                if source_bilder.supports_i128() {
                    let lo = u64::from_ne_bytes(bytes[..8].try_into().unwrap());
                    let hi = u64::from_ne_bytes(bytes[8..].try_into().unwrap());
                    format!("((__uint128_t) ({hi:x})) << 64 | ({lo:x})")
                } else {
                    panic!("emulating 128 int consts not supported ATM.")
                }
            }
        },
        TyKind::Float(float) => match float {
            FloatTy::F32 => {
                let float = f32::from_ne_bytes(bytes.try_into().unwrap());
                if float.is_infinite() {
                    if float.is_sign_positive() {
                        "1.0 / 0.0".into()
                    } else {
                        "-1.0 / 0.0".into()
                    }
                } else if float.is_nan() {
                    "0.0 / 0.0".into()
                } else {
                    format!("{float:?}f")
                }
            }
            FloatTy::F64 => {
                let float = f64::from_ne_bytes(bytes.try_into().unwrap());
                if float.is_infinite() {
                    if float.is_sign_positive() {
                        "1.0 / 0.0".into()
                    } else {
                        "-1.0 / 0.0".into()
                    }
                } else if float.is_nan() {
                    "0.0 / 0.0".into()
                } else {
                    format!("{float:?}")
                }
            }
            rustc_middle::ty::FloatTy::F16 | &rustc_middle::ty::FloatTy::F128 => {
                todo!("floating-point statics of type {float:?} are not supported ATM.")
            }
        },
        _ => {
            eprintln!("Can't init a static of type {static_ty:?} quite yet.");
            return String::new();
        }
    };
    format!("= {val}")
}
