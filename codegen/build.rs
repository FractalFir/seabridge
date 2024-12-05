use std::{
    ffi::OsStr,
    fs::DirEntry,
    io::{Read, Write},
    path::Path,
};

// Builds the dynamic test list
fn visit_dirs(dir: &Path, cb: &mut impl FnMut(&DirEntry)) -> std::io::Result<()> {
    if dir.is_dir() {
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.is_dir() {
                visit_dirs(&path, cb)?;
            } else {
                cb(&entry);
            }
        }
    }
    Ok(())
}
fn test_from_path(path: &Path) -> String {
    assert!(path.is_relative());
    let test_name = path
        .file_stem()
        .expect("Test has no name")
        .to_str()
        .unwrap();
    // Open the file to detect some config options.
    let mut file = std::fs::File::open(path).expect("Could not open test file");
    let mut file_content = String::new();
    file.read_to_string(&mut file_content)
        .expect("Could not read test file");
    let stable = if file_content.contains("//test:unstable") {
        "unstable"
    } else {
        "stable"
    };
    // Check if this file is an executable or library
    let test_body = if file_content.contains("fn main()") {
        format!("let executable = crate::test::utilis::compile_file(std::path::Path::new({path:?}), false, IS_RELEASE); crate::test::utilis::run_test(&executable);")
    } else {
        format!(
            "crate::test::utilis::compile_file(std::path::Path::new({path:?}), true, IS_RELEASE);"
        )
    };
    // Check if test contains a `main` function, and onlt run it if so.
    format!("mod {test_name}{{ mod debug{{\n  #[cfg(test)]\n  const IS_RELEASE:bool = false;\n  #[test]\n  fn {stable}(){{{test_body}}}}}\n mod release{{\n  #[cfg(test)]\n  const IS_RELEASE:bool = true;\n  #[test]\n  fn {stable}(){{{test_body}}}}}}}\n")
}
fn main() {
    // Tell Cargo that if the given file changes, to rerun this build script.
    println!("cargo::rerun-if-changed=tests");
    let mut test_string: String = "mod utilis;\n".into();
    visit_dirs(Path::new("tests"), &mut |file| {
        let path = file.path();
        if path.extension() != Some(OsStr::new("rs")) {
            eprintln!("Not a test file {file:?} {:?}", path.extension());
            return;
        }
        test_string.push_str(&test_from_path(&path));
    })
    .expect("Could not read the test directory");
    let mut out =
        std::fs::File::create("src/test.rs").expect("Could not create the test source file!");
    out.write_all(test_string.as_bytes()).unwrap();
}
