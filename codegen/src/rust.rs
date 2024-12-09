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

pub fn rust_shim<'tcx>(souce_builder:&mut CSourceBuilder<'tcx>,original_name:&str,shim_name:&str, instance:Instance<'tcx>,  tcx: TyCtxt<'tcx> ){
    //#[link_name = "sybol$$"]
}
pub fn rust_fn_def<'tcx>(souce_builder:&mut CSourceBuilder<'tcx>,shim_name:&str, instance:Instance<'tcx>,  tcx: TyCtxt<'tcx> )->String{
    todo!()
}