#![allow(dead_code)]
use rustc_middle::ty::InstanceKind;
use rustc_middle::ty::TyCtxt;

use rustc_middle::middle::codegen_fn_attrs::CodegenFnAttrFlags;
use rustc_middle::middle::exported_symbols::SymbolExportInfo;
use rustc_middle::middle::exported_symbols::SymbolExportLevel;
use rustc_middle::mir::mono::MonoItem;
use rustc_middle::mir::mono::Visibility;

use rustc_hir::LangItem;

use rustc_span::def_id::DefId;
/// Finds the default visibility of a definition
fn default_visibility(tcx: TyCtxt<'_>, id: DefId, is_generic: bool) -> Visibility {
    let export_level = if is_generic {
        // Generic functions never have export-level C.
        SymbolExportLevel::Rust
    } else {
        match tcx.reachable_non_generics(id.krate).get(&id) {
            Some(SymbolExportInfo {
                level: SymbolExportLevel::C,
                ..
            }) => SymbolExportLevel::C,
            _ => SymbolExportLevel::Rust,
        }
    };
    match export_level {
        // C-export level items remain at `Default` to allow C code to
        // access and interpose them.
        SymbolExportLevel::C => Visibility::Default,

        // For all other symbols, `default_visibility` determines which visibility to use.
        SymbolExportLevel::Rust => tcx.sess.default_visibility().into(),
    }
}
/// Finds the visibility of static
fn static_visibility(tcx: TyCtxt<'_>, can_be_internalized: &mut bool, def_id: DefId) -> Visibility {
    if tcx.is_reachable_non_generic(def_id) {
        *can_be_internalized = false;
        default_visibility(tcx, def_id, false)
    } else {
        Visibility::Hidden
    }
}
/// Gets the visibility of an item.
pub fn mono_item_visibility<'tcx>(
    tcx: TyCtxt<'tcx>,
    mono_item: &MonoItem<'tcx>,
    can_be_internalized: &mut bool,
    export_generics: bool,
) -> Visibility {
    let instance = match mono_item {
        // This is pretty complicated; see below.
        MonoItem::Fn(instance) => instance,

        // Misc handling for generics and such, but otherwise:
        MonoItem::Static(def_id) => return static_visibility(tcx, can_be_internalized, *def_id),
        MonoItem::GlobalAsm(item_id) => {
            return static_visibility(tcx, can_be_internalized, item_id.owner_id.to_def_id());
        }
    };

    let def_id = match instance.def {
        InstanceKind::Item(def_id)
        | InstanceKind::DropGlue(def_id, Some(_))
        | InstanceKind::AsyncDropGlueCtorShim(def_id, Some(_)) => def_id,

        // We match the visibility of statics here
        InstanceKind::ThreadLocalShim(def_id) => {
            return static_visibility(tcx, can_be_internalized, def_id);
        }

        // These are all compiler glue and such, never exported, always hidden.
        InstanceKind::VTableShim(..)
        | InstanceKind::ReifyShim(..)
        | InstanceKind::FnPtrShim(..)
        | InstanceKind::Virtual(..)
        | InstanceKind::Intrinsic(..)
        | InstanceKind::ClosureOnceShim { .. }
        | InstanceKind::ConstructCoroutineInClosureShim { .. }
        | InstanceKind::DropGlue(..)
        | InstanceKind::AsyncDropGlueCtorShim(..)
        | InstanceKind::CloneShim(..)
        | InstanceKind::FnPtrAddrShim(..) => return Visibility::Hidden,
    };

    // The `start_fn` lang item is actually a monomorphized instance of a
    // function in the standard library, used for the `main` function. We don't
    // want to export it so we tag it with `Hidden` visibility but this symbol
    // is only referenced from the actual `main` symbol which we unfortunately
    // don't know anything about during partitioning/collection. As a result we
    // forcibly keep this symbol out of the `internalization_candidates` set.
    //
    // FIXME: eventually we don't want to always force this symbol to have
    //        hidden visibility, it should indeed be a candidate for
    //        internalization, but we have to understand that it's referenced
    //        from the `main` symbol we'll generate later.
    //
    //        This may be fixable with a new `InstanceKind` perhaps? Unsure!
    if tcx.is_lang_item(def_id, LangItem::Start) {
        *can_be_internalized = false;
        return Visibility::Hidden;
    }

    let is_generic = instance.args.non_erasable_generics().next().is_some();

    // Upstream `DefId` instances get different handling than local ones.
    let Some(def_id) = def_id.as_local() else {
        return if export_generics && is_generic {
            // If it is an upstream monomorphization and we export generics, we must make
            // it available to downstream crates.
            *can_be_internalized = false;
            default_visibility(tcx, def_id, true)
        } else {
            Visibility::Hidden
        };
    };

    if is_generic {
        if export_generics {
            if tcx.is_unreachable_local_definition(def_id) {
                // This instance cannot be used from another crate.
                Visibility::Hidden
            } else {
                // This instance might be useful in a downstream crate.
                *can_be_internalized = false;
                default_visibility(tcx, def_id.to_def_id(), true)
            }
        } else {
            // We are not exporting generics or the definition is not reachable
            // for downstream crates, we can internalize its instantiations.
            Visibility::Hidden
        }
    } else {
        // If this isn't a generic function then we mark this a `Default` if
        // this is a reachable item, meaning that it's a symbol other crates may
        // use when they link to us.
        if tcx.is_reachable_non_generic(def_id.to_def_id()) {
            *can_be_internalized = false;
            debug_assert!(!is_generic);
            return default_visibility(tcx, def_id.to_def_id(), false);
        }

        // If this isn't reachable then we're gonna tag this with `Hidden`
        // visibility. In some situations though we'll want to prevent this
        // symbol from being internalized.
        //
        // There's two categories of items here:
        //
        // * First is weak lang items. These are basically mechanisms for
        //   libcore to forward-reference symbols defined later in crates like
        //   the standard library or `#[panic_handler]` definitions. The
        //   definition of these weak lang items needs to be referencable by
        //   libcore, so we're no longer a candidate for internalization.
        //   Removal of these functions can't be done by LLVM but rather must be
        //   done by the linker as it's a non-local decision.
        //
        // * Second is "std internal symbols". Currently this is primarily used
        //   for allocator symbols. Allocators are a little weird in their
        //   implementation, but the idea is that the compiler, at the last
        //   minute, defines an allocator with an injected object file. The
        //   `alloc` crate references these symbols (`__rust_alloc`) and the
        //   definition doesn't get hooked up until a linked crate artifact is
        //   generated.
        //
        //   The symbols synthesized by the compiler (`__rust_alloc`) are thin
        //   veneers around the actual implementation, some other symbol which
        //   implements the same ABI. These symbols (things like `__rg_alloc`,
        //   `__rdl_alloc`, `__rde_alloc`, etc), are all tagged with "std
        //   internal symbols".
        //
        //   The std-internal symbols here **should not show up in a dll as an
        //   exported interface**, so they return `false` from
        //   `is_reachable_non_generic` above and we'll give them `Hidden`
        //   visibility below. Like the weak lang items, though, we can't let
        //   LLVM internalize them as this decision is left up to the linker to
        //   omit them, so prevent them from being internalized.
        let attrs = tcx.codegen_fn_attrs(def_id);
        if attrs
            .flags
            .contains(CodegenFnAttrFlags::RUSTC_STD_INTERNAL_SYMBOL)
        {
            *can_be_internalized = false;
        }

        Visibility::Hidden
    }
}
