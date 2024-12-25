use rustc_middle::mir::mono::Linkage;
use rustc_middle::mir::mono::MonoItemData;
use rustc_middle::ty::Instance;
use rustc_middle::ty::TyCtxt;

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
        Some(Linkage::Private | Linkage::Internal) => {
            return None;
        }
        Some(Linkage::Common | Linkage::External | Linkage::AvailableExternally) | None => "extern",
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
    source_bilder.source_file_mut().push(format!("{decl};\n"));
}
