#![feature(
    rustc_private,
    iter_intersperse,
    int_roundings,
    let_chains,
    hash_set_entry
)]
#![warn(clippy::pedantic)]
#![warn(missing_docs)]
#![warn(clippy::missing_docs_in_private_items)]

//! `rustc_codegen_c` is a Rust compiler backend, capable of turing Rust MIR into C source files. The project aims to turn Rust into UB-free C,
//! taking many lessons from the previous implementation, `rustc_codegen_clr`. It is written mostly from scrath, with C in mind. The emmited C should also be human-readable,
//! with high-quality type translation, with the code preserving high-level constructs, such as loops or if's.
//!
//! The emmited `C` should also be compatible with ealier versions of `C`, using as few extensions as possible.
//!
//! The project aims to be highly configurable, allowing it to work efficently with a wide range of compilers.
//!
//! Since the goal is to write an easy-to-understand and maintaiable backend, All functions and types within the project need to be documented, and written in a straigtforward way.
//! The number of dependencies shoul also be minimal.
//!
//! The ABI of functions compiled by the projec also should closely follow, if not fully match with the Rust compiler ABI.

extern crate rustc_abi;

extern crate rustc_codegen_ssa;
extern crate rustc_const_eval;
extern crate rustc_data_structures;
extern crate rustc_driver;
extern crate rustc_errors;
extern crate rustc_hir;
extern crate rustc_index;
extern crate rustc_metadata;
extern crate rustc_middle;
extern crate rustc_session;
extern crate rustc_span;
extern crate rustc_symbol_mangling;
extern crate rustc_target;
extern crate rustc_ty_utils;
extern crate stable_mir;
/// Handles turning a Rust function into a C one, and is resposible for converting the ABI.
mod function;
/// Used for generating Rust code.
mod rust;
/// Builds the C source, and also contains the config.
mod souce_builder;
/// Handles turning a Rust static into a C one.
mod statics;
/// This module contains a list of generated test cases, created from the .rs files in `tests`.
// Auto-generated, contains no docs.
#[allow(missing_docs, clippy::missing_docs_in_private_items)]
mod test;
/// Misc utilities
mod utilis;

use rustc_codegen_ssa::back::archive::ArArchiveBuilder;
use rustc_codegen_ssa::back::archive::ArchiveBuilder;
use rustc_codegen_ssa::back::archive::ArchiveBuilderBuilder;
use rustc_codegen_ssa::traits::CodegenBackend;
use rustc_codegen_ssa::CodegenResults;
use rustc_codegen_ssa::CompiledModule;
use rustc_codegen_ssa::CrateInfo;
use rustc_codegen_ssa::ModuleKind;

use rustc_session::config::OutputFilenames;
use rustc_session::Session;

use rustc_data_structures::fx::FxIndexMap;

use rustc_middle::dep_graph::WorkProduct;
use rustc_middle::dep_graph::WorkProductId;
use rustc_middle::mir::mono::MonoItem;
use rustc_middle::ty::EarlyBinder;
use rustc_middle::ty::Instance;
use rustc_middle::ty::Ty;
use rustc_middle::ty::TyCtxt;
use rustc_middle::ty::TypeFoldable;
use rustc_middle::ty::TypingEnv;

use rustc_span::def_id::DefId;

use rustc_metadata::EncodedMetadata;

use souce_builder::CSourceBuilder;
use souce_builder::StringBuilder;

use std::path::Path;

use rustc_middle::ty::List;

use rustc_middle::ty::GenericArg;

use rustc_middle::ty::PseudoCanonicalInput;

use std::any::Any;
/// Retrives the name of a static.
fn static_ident(stotic: DefId, tcx: TyCtxt<'_>) -> String {
    instance_ident(Instance::mono(tcx, stotic), tcx)
}
/// Retrives the name of an instance. This name *must* be unqiue, altough it may be trimmed.
fn instance_ident<'tcx>(instance: Instance<'tcx>, tcx: TyCtxt<'tcx>) -> String {
    rustc_symbol_mangling::symbol_name_for_instance_in_crate(tcx, instance, instance.def_id().krate)
        .to_string()
        .replace(['.', ' '], "_")
}
#[no_mangle]
/// Entrypoint of the codegen. This function starts the backend up, and returns a reference to it to rustc.
pub extern "Rust" fn __rustc_codegen_backend() -> Box<dyn CodegenBackend> {
    Box::new(CBackend)
}
/// Packs all the files created by the codegen into an `rlib`. Highly inspired by cranelifts glue code.
struct RlibArchiveBuilder;
impl ArchiveBuilderBuilder for RlibArchiveBuilder {
    fn new_archive_builder<'a>(&self, sess: &'a Session) -> Box<dyn ArchiveBuilder + 'a> {
        Box::new(ArArchiveBuilder::new(
            sess,
            &rustc_codegen_ssa::back::archive::DEFAULT_OBJECT_READER,
        ))
    }
    fn create_dll_import_lib(
        &self,
        _sess: &Session,
        _lib_name: &str,
        _dll_imports: std::vec::Vec<rustc_codegen_ssa::back::archive::ImportLibraryItem>,
        _tmpdir: &Path,
    ) {
        unimplemented!("creating dll imports is not supported");
    }
}
/// The C backend.
struct CBackend;
impl CodegenBackend for CBackend {
    /// Used for codegen-specifc diagnostics: currently, no additional diagnositcs are implemented, so this can stay empty.
    fn locale_resource(&self) -> &'static str {
        ""
    }
    /// Compiles a given crate, turning each codegen unit into a separate `C` source file
    fn codegen_crate<'a>(
        &self,
        tcx: TyCtxt<'_>,
        metadata: EncodedMetadata,
        _need_metadata_module: bool,
    ) -> Box<dyn Any> {
        // What is this `defid_set`? The doc's don't seem to explain it too well...
        let monos = tcx.collect_and_partition_mono_items(());
        let crate_info = CrateInfo::new(tcx, "??".to_string());
        // Generate a separate source file for each cgu.
        let source_files = {
            let mut source_bilder = CSourceBuilder::new(
                tcx.sess
                    .target
                    .pointer_width
                    .try_into()
                    .expect("Targets with pointer size bigger than 256 not supported!"),
            );
            let name = crate_info.local_crate_name.to_string();
            for (item, data) in monos
                .codegen_units
                .iter()
                .flat_map(rustc_middle::mir::mono::CodegenUnit::items)
            {
                rustc_middle::ty::print::with_no_trimmed_paths! {match item {
                    MonoItem::Fn(finstance) => {
                        eprintln!("Defining function:{finstance:?}");
                        function::compile_function(*finstance, *data, &mut source_bilder, tcx,);
                    }
                    MonoItem::Static(static_def) => {
                        statics::define_static(*static_def, *data, &mut source_bilder, tcx);
                    }
                    MonoItem::GlobalAsm(asm) => {
                        eprintln!("Global asm not supported ATM. asm:{asm:?}");
                    }
                }}
            }
            (name, source_bilder.into_source_files())
        };

        Box::new((source_files, metadata, crate_info))
    }
    /// Saves the in-memory C source file
    fn join_codegen(
        &self,
        ongoing_codegen: Box<dyn Any>,
        _sess: &Session,
        outputs: &OutputFilenames,
    ) -> (CodegenResults, FxIndexMap<WorkProductId, WorkProduct>) {
        let ((name, (header, rs_bridge)), metadata, crate_info) = *ongoing_codegen
            .downcast::<(
                (String, (StringBuilder, StringBuilder)),
                EncodedMetadata,
                CrateInfo,
            )>()
            .expect("in join_codegen: ongoing_codegen is not an Assembly");
        let modules = vec![{
            use std::io::Write;
            let header_path = outputs
                .temp_path_ext("hpp", Some(&name))
                .with_file_name(&name)
                .with_extension("hpp");
            std::fs::File::create(&header_path)
                .unwrap()
                .write_all(header.bytes())
                .unwrap();
            let rust_bridge_source = outputs
                .temp_path_ext("rs_bridge", Some(&name))
                .with_file_name(&name)
                .with_extension("rs_bridge");
            std::fs::File::create(&rust_bridge_source)
                .unwrap()
                .write_all(rs_bridge.bytes())
                .unwrap();
            let out = std::process::Command::new("g++")
                .arg(&header_path)
                .arg("-fsyntax-only")
                .arg("-std=c++20")
                .output()
                .expect("Could not run a syntax check using g++");
            assert!(out.status.success(), "{out:?}");
            if std::env::var("SB_NO_COMPILE_SHIM").is_err() {
                let rust_bridge_lib = outputs
                    .temp_path_ext("elf", Some(&name))
                    .with_file_name(&name)
                    .with_extension("elf");
                let out = std::process::Command::new("rustc")
                    .arg(&rust_bridge_source)
                    .arg("-O")
                    .arg("--crate-type=staticlib")
                    .arg("-o")
                    .arg(&rust_bridge_lib)
                    .output()
                    .expect("Could not compile the Rust bridge code.");
                eprintln!(
                    "rust_bridge_source:{rust_bridge_source:?} rust_bridge_lib:{rust_bridge_lib:?}"
                );
                assert!(out.status.success(), "stdout:{} stderr:{}",String::from_utf8(out.stdout).unwrap(),String::from_utf8(out.stderr).unwrap());
                CompiledModule {
                    name,
                    kind: ModuleKind::Regular,
                    object: Some(rust_bridge_lib),
                    bytecode: Some(header_path),
                    dwarf_object: None,
                    llvm_ir: None,
                    assembly: None,
                }
            } else {
                CompiledModule {
                    name,
                    kind: ModuleKind::Regular,
                    object: None,
                    bytecode: Some(header_path),
                    dwarf_object: None,
                    llvm_ir: None,
                    assembly: Some(rust_bridge_source),
                }
            }
        }];
        let codegen_results = CodegenResults {
            modules,
            allocator_module: None,
            metadata_module: None,
            metadata,
            crate_info,
        };
        assert!(std::env::var("FORCE_FAIL").is_err());
        (codegen_results, FxIndexMap::default())
    }
    /// Collects all the files emmited by the codegen for a specific crate, and turns them into a .rlib file containing all the C source files and metadata.
    fn link(&self, sess: &Session, codegen_results: CodegenResults, outputs: &OutputFilenames) {
        use rustc_codegen_ssa::back::link::link_binary;
        link_binary(sess, &RlibArchiveBuilder, codegen_results, outputs);
        //todo!();
    }
}
/// Turns a possibly generic type `T` into a concreate one, removing lifetimes.
pub fn monomorphize<'tcx, T: TypeFoldable<TyCtxt<'tcx>> + Clone>(
    instance: Instance<'tcx>,
    ty: T,
    ctx: TyCtxt<'tcx>,
) -> T {
    instance.instantiate_mir_and_normalize_erasing_regions(
        ctx,
        TypingEnv::fully_monomorphized(),
        EarlyBinder::bind(ty),
    )
}
/// Checks the layout of this type to see if it is fat or tree.
/// # Panics
/// Panics if `ptr_type` is not a pointer.
#[must_use]
pub fn is_fat_ptr<'tcx>(
    ptr_type: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    method: rustc_middle::ty::Instance<'tcx>,
) -> bool {
    use rustc_abi::BackendRepr;
    let ptr_type = monomorphize(method, ptr_type, tcx);
    let layout = tcx
        .layout_of(rustc_middle::ty::PseudoCanonicalInput {
            typing_env: TypingEnv::fully_monomorphized(),
            value: ptr_type,
        })
        .expect("Can't get layout of a type.")
        .layout;
    let abi = layout.0 .0.backend_repr;
    match abi {
        BackendRepr::Scalar(_) => false,
        BackendRepr::ScalarPair(_, _) => true,
        _ => panic!("Unexpected abi of pointer to {ptr_type:?}. The ABI was:{abi:?}"),
    }
}
pub fn instance_try_resolve<'tcx>(
    adt: DefId,
    tcx: TyCtxt<'tcx>,
    gargs: &'tcx List<GenericArg<'tcx>>,
) -> Instance<'tcx> {
    tcx.resolve_instance_raw(PseudoCanonicalInput {
        typing_env: rustc_middle::ty::TypingEnv::fully_monomorphized(),
        value: (adt, gargs),
    })
    .unwrap()
    .unwrap()
}
