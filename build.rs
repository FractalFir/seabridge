use std::path::Path;
fn main() {
    let tests = Path::new("cargo-tests");
    let test_dir = std::fs::read_dir(tests).expect("Could not open the cargo test directory");
    let mut dirs:Vec<String> = Vec::new();
    for entry in test_dir {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.is_dir() {
            dirs.push(path.file_stem().unwrap().to_str().unwrap().to_string());
        }
    }
    panic!("dirs:{dirs:?}")
}
