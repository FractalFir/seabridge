use std::collections::HashSet;

use rustc_middle::ty::AdtKind;
use rustc_middle::ty::GenericArg;
use rustc_middle::ty::Instance;
use rustc_middle::ty::IntTy;
use rustc_middle::ty::List;
use rustc_middle::ty::Mutability;
use rustc_middle::ty::PseudoCanonicalInput;
use rustc_middle::ty::Ty;
use rustc_middle::ty::TyCtxt;
use rustc_middle::ty::TyKind;
use rustc_middle::ty::TypingEnv;
use rustc_middle::ty::UintTy;

use rustc_target::abi::FieldsShape;
use rustc_target::abi::Variants;

use rustc_abi::Size;

use crate::function::c_type_string;
use crate::function::mangle;
use crate::rust::rust_shim;
/// Builtin, layout-compatible C++ repr of a Rust array
const RUST_ARR: &str = "#ifndef RUST_ARR_DEFINED
template<typename T, uintptr_t SIZE> class RustArr{
    T arr[SIZE];
    T& operator[](uintptr_t index){
        if(index > SIZE) throw std::out_of_range(\"Index out of range in Rust array\");
        return arr[index];
    }
};
#define RUST_ARR_DEFINED 1
#endif\n";
/// Builtin, layout-compatible C++ repr of a Rust *[T].
const RUST_SLICE: &str = "#ifndef RUST_SLICE
template<typename T> class RustSlice{
    T* ptr;
    uintptr_t len;
    T& operator[](uintptr_t idx){
        if (idx > len)throw std::out_of_range(\"Index out of range in Rust slice\");
        return (ptr+idx);
    }
};
#define RUST_SLICE 1
#endif\n";
/// Builtin, layout-compatible C++ repr Rust *dyn Trait
const RUST_DYN: &str = "#ifndef RUST_DYN
class RustDyn{
    void* ptr;
    void* vtable;
};
#define RUST_DYN 1
#endif\n";
/// Builtin, layout-compatible C++ repr of Rust *str
const RUST_STR: &str = "#ifndef RUST_STR
struct RustStr{
    char32_t* utrf8_data;
    uintptr_t len;
};
#define RUST_STR 1
#endif
#ifndef RUST_FAT_PTR
template<typename T> struct RustFatPtr{
    T* data;
    void* metadata;
};
#define RUST_FAT_PTR 1
#endif\n";
const RUST_FN_DEF: &str = "
#ifndef RUST_FN_DEF
template<uint64_t id> struct RustFnDef{};
#define RUST_FN_DEF 1
#endif\n";
/// An append-only UTF-8 string.
// In the future, its append-only nature can be used for optimzations.
// For example, it could be implemented using memory pages, with flags encouraging the kernel to swap it out of ram.
// Those memmapped pages can then be diretly coppied to a file.
#[derive(Default, Debug)]
pub struct StringBuilder {
    /// Inner append-only buffer
    buffer: String,
}
impl StringBuilder {
    /// Appends a string to this builder.
    pub fn push(&mut self, s: impl AsRef<str>) {
        let s = s.as_ref();
        assert_eq!(
            s.chars().filter(|c| *c == '{').count(),
            s.chars().filter(|c| *c == '}').count()
        );
        self.buffer.push_str(s);
    }
    /// A reference to the underlying buffer, to be written to disk.
    pub fn bytes(&self) -> &[u8] {
        self.buffer.as_bytes()
    }
}
/// A special structure allowing for types to be defined.
#[derive(Debug)]
pub struct CSourceBuilder<'tcx> {
    /// The generated C++ header file
    source_file: StringBuilder,
    /// Rust bridge file
    rust_file: StringBuilder,
    /// Defines what ``size_of(uintptr_t)`` is for a given target.
    size_of_usize: u8,
    /// Lists all the headers included by this file.
    includes: HashSet<String>,
    /// Contains all already `defined` functions.
    defined: HashSet<Instance<'tcx>>,
    /// Contains all already `decalred` functions.
    decalred: HashSet<Instance<'tcx>>,
    /// Contains all types, for which a full defintion has been provided.
    defined_tys: HashSet<Ty<'tcx>>,
    /// Contains all types, for which a declaration has been provided.
    declared_tys: HashSet<Ty<'tcx>>,
    /// Types which don't yet need to be fully defined, but should be defined at some later point.
    delayed_typedefs: std::collections::vec_deque::VecDeque<Ty<'tcx>>,
}
impl<'tcx> CSourceBuilder<'tcx> {
    pub fn delayed_typedefs(&self) -> &std::collections::vec_deque::VecDeque<Ty<'tcx>> {
        &self.delayed_typedefs
    }
    /// Checks if the type defintion of a given type is already present.
    pub fn is_ty_defined(&mut self, ty: Ty<'tcx>) -> bool {
        if ty.is_primitive() {
            return true;
        }
        self.defined_tys.contains(&ty)
    }
    /// Checks if the type declaration of a given type is already present.
    pub fn is_ty_declared(&mut self, ty: Ty<'tcx>) -> bool {
        if ty.is_primitive() {
            return true;
        }
        self.declared_tys.contains(&ty)
    }
    /// Checks if a given instance is declared(it may not be defined, so its implementation may be somewhere else)
    pub fn set_declared(&mut self, instance: Instance<'tcx>) {
        self.decalred.insert(instance);
    }
    /// Checks if a given instance is defined(it's implementation is provided)
    pub fn set_defined(&mut self, instance: Instance<'tcx>) {
        self.set_declared(instance);
        assert!(self.defined.insert(instance));
    }
    /// Checks if this instance is `defined`
    pub fn is_defined(&self, instance: Instance<'tcx>) -> bool {
        self.defined.contains(&instance)
    }
    /// Checks if this instance is `declared`
    pub fn is_declared(&self, instance: Instance<'tcx>) -> bool {
        self.decalred.contains(&instance)
    }
    /// Includes the specified source file if it is not already include.
    pub fn include(&mut self, file_name: &str) {
        assert!(!file_name.contains('#'));
        assert!(!file_name.contains('<'));
        assert!(!file_name.contains('>'));
        self.source_file.push(format!("#include <{file_name}>\n"));
        assert!(self.includes.insert(file_name.to_owned()));
    }
    /// When compiling the C source file, asserts that a given condtion `cond` is true. The `message` must be a valid C identifier name.
    pub fn static_assert(&mut self, cond: &str, message: &str) {
        assert!(
            message
                .chars()
                .all(|c| c.is_alphanumeric() || c == '_' || c == '$'),
            "Invalid static assert message:{message:?}"
        );
        self.source_file
            .push(format!("typedef char assert_{message}[({cond})?1:-1];\n"));
    }
    /// A convience wrapper around [`Self::static_assert`], checking the size of a given type at compile time.
    pub fn assert_sizeof(&mut self, type_string: &str, size: u64) {
        if size == 0 {
            return;
        }
        self.static_assert(
            &format!("sizeof({type_string}) == {size}"),
            &format!(
                "sizeof_{type_string}_is_{size}",
                type_string = type_string.replace('*', "ptr").replace(' ', "_")
            ),
        );
    }
    /// A convience wrapper around [`Self::static_assert`], checking the aligement of a given type at compile time.
    pub fn assert_alignof(&mut self, type_string: &str, alignment: u64) {
        self.static_assert(
            &format!("alignof({type_string}) == {alignment}"),
            &format!(
                "alignof_{type_string}_is_{alignment}",
                type_string = type_string.replace('*', "ptr").replace(' ', "_")
            ),
        );
    }
    pub fn add_rust(&mut self, s: &str) {
        self.rust_file.push(s);
    }
    /// Creates a new source file with the specified settings.
    /// `size_of_usize`: result of computing ``size_of(uintptr_t)`` for a given target. Is in bytes.
    pub fn new(size_of_usize: u8) -> Self {
        let mut res = Self {
            size_of_usize,
            source_file: StringBuilder::default(),
            rust_file: StringBuilder::default(),
            includes: HashSet::default(),
            defined: HashSet::default(),
            decalred: HashSet::default(),
            defined_tys: HashSet::default(),
            declared_tys: HashSet::default(),
            delayed_typedefs: std::collections::vec_deque::VecDeque::with_capacity(0x100),
        };
        res.include("stdint.h");
        res.include("stdexcept");
        res.include("span");
        res.source_file.push(RUST_ARR);
        res.source_file.push(RUST_SLICE);
        res.source_file.push(RUST_DYN);
        res.source_file.push(RUST_STR);
        res.source_file.push(RUST_FN_DEF);
        res.source_file.push("struct isize{intptr_t i;};\n");
        res.source_file.push("struct usize{uintptr_t i;};\n");
        res.source_file.push("struct RustChar{uint32_t i;};\n");
        res.source_file.push("struct RustFn;\n");
        res.source_file
            .push("template<typename... Types> struct RustTuple;\n");
        res.source_file.push("#undef unix\n");

        res.source_file.push("#define restrict __restrict\n");
        res.assert_sizeof("void*", (res.size_of_usize / 8).into());
        res.source_file
            .push("struct ptr_pair{void* addr; void* meta;};\n");

        res.rust_file.push("#![feature(slice_ptr_get)]\n");
        res.rust_file.push("#[repr(C)]
pub struct RustStr{ptr:*const char, len:usize}
impl Into<*const str> for RustStr{
    fn into(self) -> *const str { 
        unsafe{std::slice::from_raw_parts(self.ptr as *const u8,self.len) as *const [u8] as *const str}
    }
}");
        res.rust_file.push(
            "#[repr(C)]
pub struct RustSlice{ptr:*const char, len:usize}
impl<T> Into<*const [T]> for RustSlice{
    fn into(self) -> *const [T]{ 
        unsafe{std::slice::from_raw_parts(self.ptr as *const T,self.len) as *const _}
    }
}",
        );
        res.rust_file.push(
            "#[repr(C)]
pub struct RustDyn{ptr:*const u8, len:usize}
impl<T> Into<*const [T]> for RustDyn{
    fn into(self) -> *const [T]{ 
        unsafe{std::slice::from_raw_parts(self.ptr as *const T,self.len) as *const _}
    }
}
impl<T> Into<RustDyn> for *const [T]{
    fn into(self) -> RustDyn{ 
        RustDyn{ptr:self.as_ptr() as *const u8, len:self.len()}
    }
}",);
        res
    }
    /// Checks if the target `C` compiler supports the `restrict` keyword.
    // TODO: check if the C version is > C99 or the compiler is known to support `restrict`.
    #[allow(clippy::unused_self)]
    pub(crate) fn supports_restrict(&self) -> bool {
        true
    }
    pub fn add_ty_templates(&mut self, t: Ty<'tcx>, tcx: TyCtxt<'tcx>) {
        use crate::rustc_middle::ty::TypeVisitable;
        t.visit_with(&mut TemplateGenerator { tcx: tcx, sb: self });
    }
    /// Adds all the typedefs this type needs.
    pub fn add_typedefs(&mut self, t: Ty<'tcx>, tcx: TyCtxt<'tcx>, instance: Instance<'tcx>) {
        self.delayed_typedefs.push_front(t);
        while let Some(t) = self.delayed_typedefs.pop_back() {
            add_ty(self, tcx, t, instance);
        }
    }
    /// Adds a new function *declaration* to this source files.
    pub fn add_fn_decl(&mut self, finstance: Instance<'tcx>, tcx: TyCtxt<'tcx>) {
        if !self.is_declared(finstance) {
            let fn_name = crate::instance_ident(finstance, tcx)
                .to_string()
                .replace('.', "_");

            let shim_symbol = create_shim(&fn_name);
            let shim_decl = crate::function::fn_decl(finstance, tcx, self, &shim_symbol);
            let body = format!(
                "{call_shim}\n",
                call_shim = crate::function::call_shim(finstance, tcx, &shim_symbol, self)
            );
            if let Some(path) = symbol_to_path(&fn_name) {
                //
                let (beg, end) = (&path[..(path.len() - 1)], &path[path.len() - 1]);
                let namespace: String = beg
                    .iter()
                    .map(std::string::String::as_str)
                    .intersperse("::")
                    .collect();
                let gargs = finstance.args;
                gargs
                    .iter()
                    .filter_map(rustc_middle::ty::GenericArg::as_type)
                    .for_each(|ty| add_ty(self, tcx, ty, finstance));
                //
                let generic_string = generic_string(gargs, tcx, self, finstance);
                // Template preifx

                let template = if generic_string.is_empty() {
                    String::new()
                } else {
                    format!("template{generic_string}")
                };

                let decl = crate::function::fn_decl(finstance, tcx, self, end);
                self.source_file
                    .push(format!("extern \"C\" {shim_decl};\nnamespace {namespace}{{ \n /*fndecl*/ {template}{decl}{{{body}}} }}\n",));
                self.source_file.push(";\n");
            } else {
                let decl = crate::function::fn_decl(finstance, tcx, self, &fn_name);
                self.source_file
                    .push(format!("extern \"C\" {shim_decl};\n"));
                self.source_file.push(&decl);
                self.source_file.push(format!("{{{body}}};\n"));
            }
            rust_shim(self, &fn_name, &shim_symbol, finstance, tcx);
            self.set_declared(finstance);
        }
    }
    /// Adds a function definition(the implementation of a function).
    pub fn add_fn_def(&mut self, finstance: Instance<'tcx>, tcx: TyCtxt<'tcx>) {
        self.add_fn_decl(finstance, tcx);
    }
    /// Checks if the generated source file ought to be only a header, and not contain any implementation.
    #[allow(clippy::unused_self)]
    pub fn header_mode(&self) -> bool {
        true
    }
    #[allow(clippy::unused_self)]
    /// Checks if the target compiler supports 128 bit ints, or if it requires emulation.
    pub fn supports_i128(&self) -> bool {
        true
    }
    #[allow(clippy::unused_self)]
    /// Checks if the target compiler supports 16 bit floats, or if it requires emulation.
    pub fn supports_f16(&self) -> bool {
        true
    }
    #[allow(clippy::unused_self)]
    /// Checks if the target compiler supports 128 bit floats, or if it requires emulation.
    pub fn supports_f128(&self) -> bool {
        true
    }
    #[allow(clippy::unused_self)]
    /// Checks if the target compiler supports the alignment attribute, or if it requires emulation.
    pub fn is_align_attr_supported(&self) -> bool {
        true
    }
    /// Truns `self` into the underlying C source file buffer.
    pub fn into_source_files(mut self) -> (StringBuilder, StringBuilder) {
        self.source_file
            .push("#ifdef __unix__\n#define unix 1\n#endif\n");
        (self.source_file, self.rust_file)
    }
    /// Returns a mutable reference to the backing buffer.
    pub fn source_file_mut(&mut self) -> &mut StringBuilder {
        &mut self.source_file
    }
    #[allow(clippy::unused_self)]
    /// Checks if the target compiler supports the section attribute
    pub fn supports_section(&self) -> bool {
        true
    }
    pub fn delay_typedef(&mut self, ty: Ty<'tcx>) {
        self.delayed_typedefs.push_front(ty);
    }
    /*
    fn add_fn_template(&mut self, genetric_fn: Instance<'tcx>, tcx: TyCtxt<'tcx>) {
        if self.is_declared(genetric_fn) {
            return;
        }
        let fn_name = crate::instance_ident(genetric_fn, tcx)
            .to_string()
            .replace('.', "_");
        let Some(path) = symbol_to_path(&fn_name) else {
            panic!()
        };
        let (beg, end) = (&path[..(path.len() - 1)], &path[path.len() - 1]);
        let namespace: String = beg
            .iter()
            .map(std::string::String::as_str)
            .intersperse("::")
            .collect();
        let mut t_idx = 0;
        let mut c_idx = 0;
        let garg_body: String = genetric_fn
            .args
            .iter()
            .filter_map(|garg| {
                if let Some(ty) = garg.as_type() {
                    let ts = format!("typename T{t_idx}");
                    t_idx += 1;
                    Some(ts)
                } else {
                    let cst = garg.as_const()?;

                    let cs = format!("typename TC{c_idx}, TC{c_idx} C{c_idx}");
                    c_idx += 1;
                    Some(cs)
                }
            })
            .intersperse(",".to_string())
            .collect();
        let template = if garg_body.is_empty() {
            "".into()
        } else {
            format!("template<{garg_body}> ")
        };
        // TODO: fullt
        let decl = format!(""end;
        self.source_file.push(format!(
            "namespace {namespace}{{ /*fn template*/
{template}{decl}; }}",
        ));
        self.source_file.push(";\n");
        self.set_declared(genetric_fn);
    }*/
}
/// Adds the type `t` to `sb`
#[allow(clippy::too_many_lines)]
fn add_ty<'tcx>(
    sb: &mut CSourceBuilder<'tcx>,
    tcx: TyCtxt<'tcx>,
    t: Ty<'tcx>,
    instance: Instance<'tcx>,
) {
    // If this type is fully defined, then its children must be fully defined, so this is OK.
    let t = crate::monomorphize(instance, t, tcx);
    sb.add_ty_templates(t, tcx);
    if sb.is_ty_defined(t) {
        return;
    }

    match t.kind() {
        TyKind::FnDef(_, _) => (),
        TyKind::Ref(_, inner, _) | TyKind::RawPtr(inner, _) => {
            if !crate::is_fat_ptr(t, tcx, instance) {
                sb.delay_typedef(*inner);
            }
        }
        TyKind::Array(elem, len) => {
            add_ty(sb, tcx, *elem, instance);
        }
        TyKind::Tuple(elems) => {
            sb.defined_tys.insert(t);
            let layout = tcx
                .layout_of(PseudoCanonicalInput {
                    typing_env: TypingEnv::fully_monomorphized(),
                    value: t,
                })
                .expect("Could not compute the layout of a type.");
            let Variants::Single { index: _ } = layout.variants else {
                panic!("Tuple must have single variant layout.");
            };
            let name = mangle(t, tcx);
            match &layout.fields {
                FieldsShape::Primitive => {
                    panic!("type {name} has primitive layout, but is not primitive.")
                }
                FieldsShape::Arbitrary {
                    offsets,
                    memory_index: _,
                } => {
                    let mut offsets: Vec<(_, _)> = offsets.clone().into_iter_enumerated().collect();
                    // Sort fields by their offset, to guarantee they are in C order.
                    offsets.sort_by(|(_, offset_a), (_, offset_b)| offset_a.cmp(offset_b));
                    let mut last_offset = Size::from_bytes(0);
                    let mut pad_count = 0;
                    let mut fields = String::new();
                    for (field_idx, offset) in &offsets {
                        if *offset != last_offset {
                            assert!(offset.bytes() > last_offset.bytes());
                            fields.push_str(&format!(
                                "uint8_t pad_{pad_count}[{}];\n",
                                offset.bytes() - last_offset.bytes()
                            ));
                            pad_count += 1;
                        }
                        let field_ty = elems[field_idx.as_usize()];
                        add_ty(sb, tcx, field_ty, instance);
                        if is_zst(crate::monomorphize(instance, field_ty, tcx), tcx) {
                            continue;
                        }
                        let field_name = format!("f{}", field_idx.as_usize());
                        fields.push_str(&format!(
                            "{} {field_name};\n",
                            c_type_string(field_ty, tcx, sb, instance)
                        ));
                        last_offset = *offset + size(field_ty, tcx);
                    }
                    let generic_string: String = elems
                        .iter()
                        .map(|ty| generic_ty_string(ty, tcx, sb, instance))
                        .intersperse(",".into())
                        .collect();
                    let generic_string = format!("<{generic_string}>");
                    let align = if sb.is_align_attr_supported() {
                        format!("__attribute__ ((aligned ({})))\n", layout.align.abi.bytes())
                    } else {
                        String::new()
                    };
                    sb.source_file.push(format!(
                        "template <> struct {align}RustTuple{generic_string}{{\n{fields}}};\n"
                    ));
                }
                //FieldsShape::Union =>
                _ => todo!("Unhandled field layout: {:?}", layout.fields),
            }
        }
        TyKind::FnPtr(binder, _) => {
            let fn_ptr =
                tcx.normalize_erasing_late_bound_regions(TypingEnv::fully_monomorphized(), *binder);

            fn_ptr.inputs().iter().for_each(|t| sb.delay_typedef(*t));
            sb.delay_typedef(fn_ptr.output());
        }
        TyKind::Closure(did, gargs) => {
            let adt_instance =
                Instance::try_resolve(tcx, TypingEnv::fully_monomorphized(), *did, gargs)
                    .unwrap()
                    .unwrap();
            // Get the mangled path: it is absolute, and not poluted by types being rexported
            let name = crate::instance_ident(adt_instance, tcx).replace('.', "_");
            let layout = tcx
                .layout_of(PseudoCanonicalInput {
                    typing_env: TypingEnv::fully_monomorphized(),
                    value: t,
                })
                .expect("Could not compute the layout of a type.");

            match &layout.variants {
                    Variants::Single { index: _ } => match &layout.fields{
                        FieldsShape::Primitive => panic!("type {name} has primitive layout, but is not primitive."),
                        FieldsShape::Arbitrary{offsets, memory_index:_}=>{
                            let clsoure_fields:Vec<_> = gargs. as_closure().upvar_tys().to_vec();
                            let mut offsets:Vec<(_,_)> = offsets.clone().into_iter_enumerated().collect();
                            // Sort fields by their offset, to guarantee they are in C order.
                            offsets.sort_by(|(_,offset_a),(_,offset_b)|offset_a.cmp(offset_b));
                            let mut last_offset = Size::from_bytes(0);
                            let mut pad_count = 0;
                            let mut fields = String::new();
                            for (field_idx,offset) in &offsets{
                                if *offset != last_offset{
                                    assert!(offset.bytes() > last_offset.bytes());
                                    fields.push_str(&format!("uint8_t pad_{pad_count}[{}];\n",offset.bytes() - last_offset.bytes()));
                                    pad_count += 1;
                                }
                                let field_ty = &clsoure_fields[field_idx.as_usize()];
                                add_ty(sb,tcx,*field_ty,instance);
                                if is_zst( crate::monomorphize(instance, *field_ty, tcx),tcx){
                                    continue;
                                }
                                let field_name = format!("f{}",field_idx.as_usize());
                                fields.push_str(&format!("{} {field_name};\n",c_type_string(*field_ty, tcx, sb,instance)));
                                last_offset = *offset + size(*field_ty,tcx);
                            }
                            let align = if sb.is_align_attr_supported(){
                                format!("__attribute__ ((aligned ({})))\n",layout.align.abi.bytes())
                            } else{
                               String::new()
                            };
                            sb.source_file.push(format!("struct {align}{name}{{\n{fields}}};\n"));
                            sb.assert_sizeof(&format!("struct {name}"), layout.size.bytes());
                            sb.assert_alignof(&format!("struct {name}"), layout.align.abi.bytes());
                        }
                        //FieldsShape::Union => 
                        _=>todo!("Unhandled field layout: {:?}",layout.fields),
                    }
                    Variants::Multiple {
                        tag,
                        tag_encoding,
                        tag_field,
                        variants,
                    } => todo!(
                        "Can't encode enums quite yet! {tag:?} {tag_encoding:?} {tag_field:?} {variants:?}"
                    ),
                }
        }
        TyKind::Dynamic(_, _, _) => {
            let name = mangle(t, tcx);
            sb.source_file.push(format!("struct {name}{{}};\n"));
        }
        TyKind::Adt(def, gargs) => {
            let adt_instance =
                Instance::try_resolve(tcx, TypingEnv::fully_monomorphized(), def.did(), gargs)
                    .unwrap()
                    .unwrap();
            let poly_gargs = List::<rustc_middle::ty::GenericArg<'_>>::identity_for_item(
                tcx,
                adt_instance.def_id(),
            );
            add_ty_template(
                sb,
                tcx,
                Instance::new(adt_instance.def_id(), poly_gargs)
                    .ty(tcx, TypingEnv::fully_monomorphized()),
            );
            // Get the mangled path: it is absolute, and not poluted by types being rexported
            let name = crate::instance_ident(adt_instance, tcx);
            let layout = tcx
                .layout_of(PseudoCanonicalInput {
                    typing_env: TypingEnv::fully_monomorphized(),
                    value: t,
                })
                .expect("Could not compute the layout of a type.");
            let generics = generic_string(gargs, tcx, sb, instance);
            let template_preifx = if generics.is_empty() {
                ""
            } else {
                "template<> "
            };

            match &layout.variants {
                Variants::Single { index: _ } => match &layout.fields {
                    FieldsShape::Primitive if is_zst(t, tcx) => {
                        if let Some(path) = symbol_to_path(&name) {
                            let (beg, end) = (&path[..(path.len() - 1)], &path[path.len() - 1]);
                            let namespace: String =
                                beg.iter().map(|s| s.as_str()).intersperse("::").collect();

                            sb.source_file.push(format!(
                                "\n#ifndef _RUST_TY_DEF_{name}\nnamespace {namespace} {{{template_preifx}struct {end}{generics}{{}};}}\n#define _RUST_TY_DEF_{name} 1\n#endif\n"
                            ));
                        } else {
                            sb.source_file
                                .push(format!("\n#ifndef _RUST_TY_DEF_{name} {template_preifx}struct {name}{generics}{{}};#define _RUST_TY_DEF_{name} 1\n#endif\n"));
                            sb.assert_sizeof(&format!("struct {name}"), layout.size.bytes());
                            sb.assert_alignof(&format!("struct {name}"), layout.align.abi.bytes());
                        }
                    }
                    FieldsShape::Primitive => {
                        panic!("type {name} has primitive layout, but is not primitive.")
                    }
                    FieldsShape::Arbitrary {
                        offsets,
                        memory_index: _,
                    } => {
                        let variant_0 = def.variants().iter().next().unwrap();
                        let mut offsets: Vec<(_, _)> =
                            offsets.clone().into_iter_enumerated().collect();
                        // Sort fields by their offset, to guarantee they are in C order.
                        offsets.sort_by(|(_, offset_a), (_, offset_b)| offset_a.cmp(offset_b));
                        let mut last_offset = Size::from_bytes(0);
                        let mut pad_count = 0;
                        let mut fields = String::new();
                        for (field_idx, offset) in &offsets {
                            if *offset != last_offset {
                                assert!(offset.bytes() > last_offset.bytes());
                                fields.push_str(&format!(
                                    "uint8_t pad_{pad_count}[{}];\n",
                                    offset.bytes() - last_offset.bytes()
                                ));
                                pad_count += 1;
                            }
                            let field_def = &variant_0.fields[*field_idx];
                            add_ty(sb, tcx, field_def.ty(tcx, gargs), instance);
                            if is_zst(
                                crate::monomorphize(instance, field_def.ty(tcx, gargs), tcx),
                                tcx,
                            ) {
                                continue;
                            }
                            let field_name = if field_def
                                .name
                                .to_string()
                                .chars()
                                .next()
                                .is_some_and(|c| c.is_alphabetic())
                            {
                                field_def.name.to_string()
                            } else {
                                format!("f{}", field_def.name)
                            };
                            fields.push_str(&format!(
                                "{} {field_name};\n",
                                c_type_string(field_def.ty(tcx, gargs), tcx, sb, instance)
                            ));
                            last_offset = *offset + size(field_def.ty(tcx, gargs), tcx);
                        }
                        let align = if sb.is_align_attr_supported() {
                            format!("__attribute__ ((aligned ({})))\n", layout.align.abi.bytes())
                        } else {
                            String::new()
                        };
                        if let Some(path) = symbol_to_path(&name) {
                            let (beg, end) = (&path[..(path.len() - 1)], &path[path.len() - 1]);
                            let namespace: String =
                                beg.iter().map(|s| s.as_str()).intersperse("::").collect();
                            if def.adt_kind() == AdtKind::Enum {
                                sb.source_file.push(format!(
                                    "#ifndef _RUST_TY_DEF_{name}\nnamespace {namespace} {{{template_preifx}union {align}{end}{generics}{{\n{fields}}};}}\n#define _RUST_TY_DEF_{name} 1\n#endif\n"
                                ));
                            } else {
                                sb.source_file.push(format!(
                                    "#ifndef _RUST_TY_DEF_{name}\nnamespace {namespace} {{{template_preifx}struct {align}{end}{generics}{{\n{fields}}};}}\n#define _RUST_TY_DEF_{name} 1\n#endif\n"
                                ));
                            }
                        } else {
                            sb.source_file.push(format!(
                                "#ifndef _RUST_TY_DEF_{name}\n{template_preifx}struct {align}{name}{generics}{{\n{fields}}};\n#define _RUST_TY_DEF_{name} 1\n#endif\n"
                            ));
                            sb.assert_sizeof(&format!("struct {name}"), layout.size.bytes());
                            sb.assert_alignof(&format!("struct {name}"), layout.align.abi.bytes());
                        }
                    }
                    FieldsShape::Union(_) => {
                        let mut fields = String::new();
                        for field_def in def.all_fields() {
                            add_ty(sb, tcx, field_def.ty(tcx, gargs), instance);
                            if is_zst(
                                crate::monomorphize(instance, field_def.ty(tcx, gargs), tcx),
                                tcx,
                            ) {
                                continue;
                            }
                            let field_name = if field_def
                                .name
                                .to_string()
                                .chars()
                                .next()
                                .is_some_and(|c| !c.is_whitespace())
                            {
                                format!("f{}", field_def.name)
                            } else {
                                field_def.name.to_string()
                            };
                            fields.push_str(&format!(
                                "{} {field_name};\n",
                                c_type_string(field_def.ty(tcx, gargs), tcx, sb, instance)
                            ));
                        }
                        let align = if sb.is_align_attr_supported() {
                            format!("__attribute__ ((aligned ({})))\n", layout.align.abi.bytes())
                        } else {
                            String::new()
                        };
                        if let Some(path) = symbol_to_path(&name) {
                            let (beg, end) = (&path[..(path.len() - 1)], &path[path.len() - 1]);
                            let namespace: String =
                                beg.iter().map(|s| s.as_str()).intersperse("::").collect();
                            sb.source_file.push(format!(
                                "#ifndef _RUST_TY_DEF_{name}\nnamespace {namespace} {{{template_preifx}union {align}{end}{generics}{{\n{fields}}};}}\n#define _RUST_TY_DEF_{name} 1\n#endif\n"
                            ));
                        } else {
                            sb.source_file.push(format!(
                                "#ifndef _RUST_TY_DEF_{name}\n{template_preifx}union {align}{name}{generics}{{\n{fields}}};\n#define _RUST_TY_DEF_{name} 1\n#endif\n"
                            ));
                            sb.assert_sizeof(&format!("union {name}"), layout.size.bytes());
                            sb.assert_alignof(&format!("union {name}"), layout.align.abi.bytes());
                        }
                    }
                    FieldsShape::Array { .. } => {
                        todo!("Unhandled field layout: {:?}", layout.fields)
                    }
                },
                Variants::Multiple {
                    tag,
                    tag_encoding: _,
                    tag_field: _,
                    variants,
                } => {
                    assert_eq!(variants.len(), def.variants().len());
                    let varaint_string: String = variants
                        .into_iter()
                        .zip(def.variants())
                        .map(|(layout_variant, adt_variant)| {
                            match &layout_variant.fields {
                                FieldsShape::Primitive => panic!(
                                    "type {name} has primitive layout, but is not primitive."
                                ),
                                FieldsShape::Arbitrary {
                                    offsets,
                                    memory_index: _,
                                } => {
                                    let mut offsets: Vec<(_, _)> =
                                        offsets.clone().into_iter_enumerated().collect();
                                    // Sort fields by their offset, to guarantee they are in C order.
                                    offsets.sort_by(|(_, offset_a), (_, offset_b)| {
                                        offset_a.cmp(offset_b)
                                    });
                                    let mut last_offset = Size::from_bytes(0);
                                    let mut pad_count = 0;
                                    let mut fields = String::new();
                                    for (field_idx, offset) in &offsets {
                                        if *offset != last_offset {
                                            assert!(offset.bytes() > last_offset.bytes());
                                            fields.push_str(&format!(
                                                "uint8_t pad_{pad_count}[{}];\n",
                                                offset.bytes() - last_offset.bytes()
                                            ));
                                            pad_count += 1;
                                        }
                                        let field_def = &adt_variant.fields[*field_idx];
                                        add_ty(sb, tcx, field_def.ty(tcx, gargs), instance);
                                        if is_zst(field_def.ty(tcx, gargs), tcx) {
                                            continue;
                                        }
                                        let field_name = if field_def
                                            .name
                                            .to_string()
                                            .chars()
                                            .next()
                                            .is_some_and(|c| !c.is_whitespace())
                                        {
                                            format!("f{}", field_def.name)
                                        } else {
                                            field_def.name.to_string()
                                        };
                                        fields.push_str(&format!(
                                            "{} {field_name};\n",
                                            c_type_string(
                                                field_def.ty(tcx, gargs),
                                                tcx,
                                                sb,
                                                instance
                                            )
                                        ));
                                        last_offset = *offset + size(field_def.ty(tcx, gargs), tcx);
                                    }
                                    let variant_name = adt_variant.name.to_string();
                                    if offsets.is_empty() {
                                        String::new()
                                    } else {
                                        format!("struct{{\n{fields}}} {variant_name};\n")
                                    }
                                }
                                _ => todo!("Unhandled variant layout: {:?}", layout.fields),
                            }
                        })
                        .collect();
                    let tag_type = primitive_to_type(tag.primitive());
                    assert_eq!(
                        layout.fields.count(),
                        1,
                        "enum contains only one variant-shared field: tag."
                    );
                    let tag_offset = layout.fields.offset(0).bytes();
                    let align = if sb.is_align_attr_supported() {
                        format!("__attribute__ ((aligned ({})))\n", layout.align.abi.bytes())
                    } else {
                        String::new()
                    };
                    let pad = if tag_offset == 0 {
                        String::new()
                    } else {
                        format!("uint8_t tag_pad[{tag_offset}];")
                    };
                    if let Some(path) = symbol_to_path(&name) {
                        let (beg, end) = (&path[..(path.len() - 1)], &path[path.len() - 1]);
                        let namespace: String =
                            beg.iter().map(|s| s.as_str()).intersperse("::").collect();
                        sb.source_file
                            .push(format!("#ifndef _RUST_TY_DEF_{name}\nnamespace {namespace}{{{template_preifx}union {align}{end}{generics}{{\n{varaint_string}\nstruct{{{pad}{tag_type} tag;}} tag;\n}};\n}}\n#define _RUST_TY_DEF_{name} \n#endif\n"));
                    } else {
                        sb.source_file
                        .push(format!("#ifndef _RUST_TY_DEF_{name}\n{template_preifx}union {align}{name}{generics}{{\n{varaint_string}\nstruct{{{pad}{tag_type} tag;}} tag;\n}};\n#define _RUST_TY_DEF_{name} 1\n#endif\n"));
                        sb.assert_sizeof(&format!("union {name}"), layout.size.bytes());
                        sb.assert_alignof(&format!("union {name}"), layout.align.abi.bytes());
                    }
                }
            }
        }
        TyKind::Never => (),
        TyKind::Slice(elem) => {
            let name = mangle(t, tcx);
            sb.source_file.push(format!("struct {name} {{}};\n"));
            add_ty(sb, tcx, *elem, instance)
        }
        TyKind::Str => {
            sb.source_file.push(format!("struct ss {{}};\n"));
        }
        _ => todo!("Can't add the relevant typedefs for type {:?}", t.kind()),
    }
    sb.defined_tys.insert(t);
}
pub fn create_shim(fn_name: &str) -> String {
    const SHIM_NAME: &str = "rust_to_c_shim";
    if !fn_name.ends_with('E') {
        return format!("{fn_name}{SHIM_NAME}");
    }
    if fn_name.len() < 20 {
        return format!("{fn_name}{SHIM_NAME}");
    }
    let hash_len = &fn_name[(fn_name.len() - 20)..(fn_name.len() - 18)];
    if hash_len != "17" {
        return format!("{fn_name}{SHIM_NAME}");
    }
    let (prefix, generic_hash) = fn_name.split_at(fn_name.len() - 17 - 2 - 1);
    format!(
        "{prefix}{shim_len}{SHIM_NAME}{generic_hash}",
        shim_len = SHIM_NAME.len()
    )
}
pub fn add_ty_template<'tcx>(
    sb: &mut CSourceBuilder<'tcx>,
    tcx: TyCtxt<'tcx>,
    generic_t: Ty<'tcx>,
) {
    if sb.is_ty_declared(generic_t) {
        return;
    }

    match generic_t.kind() {
        TyKind::Adt(def, gargs) => {
            assert!(sb.declared_tys.insert(generic_t));

            let adt_instance =
                Instance::try_resolve(tcx, TypingEnv::fully_monomorphized(), def.did(), gargs)
                    .unwrap()
                    .unwrap();
            // Get the mangled path: it is absolute, and not poluted by types being rexported
            let name = crate::instance_ident(adt_instance, tcx);
            let ty_type = match def.adt_kind() {
                rustc_middle::ty::AdtKind::Struct => "struct",
                rustc_middle::ty::AdtKind::Union => "union",
                rustc_middle::ty::AdtKind::Enum if def.variants().len() > 0 => "union",
                rustc_middle::ty::AdtKind::Enum => "struct",
            };
            let mut t_idx = 0;
            let mut c_idx = 0;
            let garg_body: String = gargs
                .iter()
                .filter_map(|garg| {
                    if let Some(ty) = garg.as_type() {
                        let ts = format!("typename T{t_idx}");
                        t_idx += 1;
                        Some(ts)
                    } else {
                        let cst = garg.as_const()?;

                        let cs = format!("typename TC{c_idx}, TC{c_idx} C{c_idx}");
                        c_idx += 1;
                        Some(cs)
                    }
                })
                .intersperse(",".to_string())
                .collect();
            let template = if garg_body.is_empty() {
                "".into()
            } else {
                format!("template<{garg_body}> ")
            };

            if let Some(path) = symbol_to_path(&name) {
                let (beg, end) = (&path[..(path.len() - 1)], &path[path.len() - 1]);
                let namespace: String = beg.iter().map(|s| s.as_str()).intersperse("::").collect();
                sb.source_file.push(format!(
                    "\nnamespace {namespace} {{{template}{ty_type} {end};}}\n"
                ));
            } else {
                sb.source_file
                    .push(format!("\n{template}{ty_type} {name};\n"));
            }
        }
        _ => (),
    }
}
/// Calculates the size of a type
pub fn size<'tcx>(ty: Ty<'tcx>, tcx: TyCtxt<'tcx>) -> Size {
    tcx.layout_of(PseudoCanonicalInput {
        typing_env: TypingEnv::fully_monomorphized(),
        value: ty,
    })
    .expect("Could not compute the layout of a type.")
    .size
}
/// Checks if a type is zero sized, and should be skipped.
pub fn is_zst<'tcx>(ty: Ty<'tcx>, tcx: TyCtxt<'tcx>) -> bool {
    tcx.layout_of(PseudoCanonicalInput {
        typing_env: TypingEnv::fully_monomorphized(),
        value: ty,
    })
    .expect("Could not compute the layout of a type.")
    .is_zst()
}
/// Converts a Primtive to a C type string.
fn primitive_to_type(primitive: rustc_target::abi::Primitive) -> &'static str {
    use rustc_target::abi::Integer;
    use rustc_target::abi::Primitive;
    match primitive {
        Primitive::Int(int, sign) => match (int, sign) {
            (Integer::I8, true) => "int8_t",
            (Integer::I16, true) => "int16_t",
            (Integer::I32, true) => "int32_t",
            (Integer::I64, true) => "int64_t",
            (Integer::I128, true) => "__int128_t",
            (Integer::I8, false) => "uint8_t",
            (Integer::I16, false) => "uint16_t",
            (Integer::I32, false) => "uint32_t",
            (Integer::I64, false) => "uint64_t",
            (Integer::I128, false) => "__uint128_t",
        },
        Primitive::Float(rustc_abi::Float::F16) => todo!(),
        Primitive::Float(rustc_abi::Float::F32) => "float",
        Primitive::Float(rustc_abi::Float::F64) => "double",
        Primitive::Float(rustc_abi::Float::F128) => todo!("No support for 128 bit floats yet!"),
        Primitive::Pointer(_) => "void*",
    }
}
pub fn symbol_to_path(symbol: &str) -> Option<Vec<String>> {
    let demangled = format!("{:#}", rustc_demangle::demangle(symbol));
    if !demangled.contains(['.', '>', '{', '}']) {
        Some(
            demangled
                .split("::")
                .map(|e| {
                    let res = e.to_string();
                    match e {
                        "float" => "_float".to_string(),
                        "int" => "_int".to_string(),
                        "char" => "_char".to_string(),
                        "private" => "_private".to_string(),
                        _ => res,
                    }
                })
                .collect(),
        )
    } else {
        None
    }
}
pub fn generic_ty_string<'tcx>(
    ty: Ty<'tcx>,
    tcx: TyCtxt<'tcx>,
    source_builder: &mut crate::souce_builder::CSourceBuilder<'tcx>,
    instance: Instance<'tcx>,
) -> String {
    match ty.kind() {
        TyKind::Uint(UintTy::Usize) => "usize".into(),
        TyKind::Int(IntTy::Isize) => "isize".into(),
        TyKind::Char => "RustChar".into(),
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
                        let tpe = generic_ty_string(*elem, tcx, source_builder, instance);
                        format!("RustSlice<{tpe}>")
                    }
                    TyKind::Dynamic(_, _, _) => format!("RustDyn"),
                    _ => format!(
                        "RustFatPtr<{inner}>",
                        inner = generic_ty_string(*inner, tcx, source_builder, instance)
                    ),
                }
            } else if is_zst(*inner, tcx) {
                format!("void {mutability}*")
            } else {
                format!(
                    "{} {mutability}*",
                    generic_ty_string(*inner, tcx, source_builder, instance)
                )
            }
        }
        _ => {
            source_builder.delay_typedef(ty);
            c_type_string(ty, tcx, source_builder, instance)
        }
    }
}
pub fn generic_string<'tcx>(
    list: &'tcx List<GenericArg<'tcx>>,
    tcx: TyCtxt<'tcx>,
    source_builder: &mut crate::souce_builder::CSourceBuilder<'tcx>,
    finstance: Instance<'tcx>,
) -> String {
    let garg_body: String = list
        .iter()
        .filter_map(|garg| {
            if let Some(ty) = garg.as_type() {
                Some(generic_ty_string(ty, tcx, source_builder, finstance))
            } else {
                let cst = garg.as_const()?;
                let (val_tree, ty) = cst.to_valtree();
                let c_type = generic_ty_string(ty, tcx, source_builder, finstance);
                let val = match ty.kind() {
                    TyKind::Bool => {
                        format!("{}", val_tree.try_to_scalar().unwrap().to_bool().unwrap())
                    }
                    TyKind::Uint(UintTy::Usize) => format!(
                        "usize{{{val}}}",
                        val = val_tree
                            .try_to_scalar()
                            .unwrap()
                            .to_target_usize(&tcx)
                            .unwrap()
                    ),
                    //TyKind::Uint(_)=>format!("{:x}",val_tree.try_to_scalar().unwrap().to_u128().unwrap()).into(),
                    //TyKind::Int(_)=>format!("{:x}",val_tree.try_to_scalar().unwrap().to_i128().unwrap()).into(),
                    _ => {
                        use std::hash::Hash;
                        use std::hash::Hasher;
                        use std::hash::SipHasher;

                        let mut hasher = SipHasher::new_with_keys(0xDEAD_C0FFE, 0xBEEF_BABE);
                        val_tree.hash(&mut hasher);
                        format!("{:#}", hasher.finish() as u64)
                    }
                };
                Some(format!("{c_type}, {val}"))
            }
        })
        .intersperse(",".to_string())
        .collect();
    if garg_body.is_empty() {
        garg_body
    } else {
        format!("<{garg_body}>")
    }
}
struct TemplateGenerator<'tcx, 'sb> {
    tcx: TyCtxt<'tcx>,
    sb: &'sb mut CSourceBuilder<'tcx>,
}
impl<'tcx> rustc_middle::ty::TypeVisitor<TyCtxt<'tcx>> for TemplateGenerator<'tcx, '_> {
    fn visit_ty(&mut self, t: Ty<'tcx>) -> Self::Result {
        use crate::rustc_middle::ty::TypeVisitable;

        match t.kind() {
            TyKind::Adt(def, gargs) => {
                self.sb.source_file.push(&format!("// {gargs:?}\n"));
                let adt_instance = Instance::try_resolve(
                    self.tcx,
                    TypingEnv::fully_monomorphized(),
                    def.did(),
                    gargs,
                )
                .unwrap()
                .unwrap();
                let poly_gargs = List::<rustc_middle::ty::GenericArg<'_>>::identity_for_item(
                    self.tcx,
                    adt_instance.def_id(),
                );
                let generic_t = Instance::new(adt_instance.def_id(), poly_gargs)
                    .ty(self.tcx, TypingEnv::fully_monomorphized());
                if self.sb.is_ty_declared(generic_t) {
                    return;
                }
                for generic in gargs.as_slice() {
                    if let Some(ty) = generic.as_type() {
                        ty.visit_with(self);
                    }
                }
                add_ty_template(self.sb, self.tcx, generic_t);
            }
            _ => (),
        }
    }
}
