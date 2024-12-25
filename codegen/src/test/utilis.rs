#![cfg(test)]
use std::{
    path::{Path, PathBuf},
    sync::LazyLock,
};
/// Builds the codegen backend, returning a relative path to it. May only be called directly from the test harness.
fn build_backend() -> &'static Path {
    fn build_internal() -> &'static Path {
        let mut cmd = std::process::Command::new("cargo");
        cmd.arg("build");
        // Build a matching version of the codegen. If the tests are run in release, use the release version, otherwise use debug.
        if !cfg!(debug_assertions) {
            cmd.arg("release");
        }
        // Run cargo to build the backend. If it fails, print that error message.
        let out = cmd
            .output()
            .expect("Could not run cargo to build the backend!");
        assert!(out.status.success(), "Failed  to build the backend");
        // Computes the target-specifc, relative the path to the codegen backend. \Supported OSs: Linux, MacOS, Windows.
        #[cfg(all(target_family = "unix", not(target_os = "macos")))]
        let res = Path::new(if cfg!(debug_assertions) {
            "../target/debug/librustc_codegen_c.so"
        } else {
            "../target/release/librustc_codegen_c.so"
        });
        #[cfg(target_os = "macos")]
        let res = Path::new(if cfg!(debug_assertions) {
            "../target/debug/librustc_codegen_c.dylib"
        } else {
            "../target/release/librustc_codegen_c.dylib"
        });
        #[cfg(target_os = "windows")]
        let res = Path::new(if cfg!(debug_assertions) {
            "../target/debug/librustc_codegen_c.dll"
        } else {
            "../target/release/librustc_codegen_c.dll"
        });
        #[cfg(not(any(target_os = "windows", target_family = "unix")))]
        compile_error!("Unsported host OS.");
        res
    }
    static BUILD_STATUS: LazyLock<&'static Path> = LazyLock::new(build_internal);
    *BUILD_STATUS
}
/// Returns an absolute path to the default or user-specified(via `CC` enviroment variable) c compiler.
/// The OS C compiler can also serve as a linker. By replacing the linker with a C compiler, we can "link" c files by compiling them.
pub fn linker() -> &'static Path {
    static C_COMPILER: LazyLock<String> =
        LazyLock::new(|| std::env::var("CC").unwrap_or("cc".into()));
    C_COMPILER.as_ref()
}
/// Compiles any given `.rs` file, with the specified optimzation level, and additonal flags
pub fn compile_file(path: &Path, is_lib: bool, is_release: bool) -> PathBuf {
    // Preparares the right `rustc` flags, with a specified backend and linker(C compiler).
    let backend = build_backend();
    let linker = linker();
    let mut cmd = std::process::Command::new("rustc");
    cmd.arg("-Z");
    cmd.arg(format!(
        "codegen_backend={backend}",
        backend = backend.display()
    ));
    cmd.arg("-C");
    cmd.arg(format!("linker={}", linker.display()));
    cmd.arg("-Zinline-mir=no");
    cmd.arg("-Cinline-threshold=999999999");
    cmd.arg(path);
    cmd.arg("--edition=2021");
    //cmd.arg("-Z");
    //cmd.arg("unstable-options");
    // Calculates the outputfile path.
    let out_path = if is_release {
        path.to_owned()
    } else {
        path.with_file_name(format!(
            "{}_debug",
            path.file_stem().unwrap().to_str().unwrap().to_owned()
        ))
    };
    let out_path = if is_lib {
        out_path.with_extension(NATIVE_LIB_EXTENSION)
    } else {
        out_path.with_extension(NATIVE_EXEC_EXTENSION)
    };
    // If this is a library, compile the output to an object file.
    if is_lib {
        cmd.arg("--crate-type=staticlib");
    }
    cmd.arg("-o");
    cmd.arg(&out_path);
    // Sets the right flags for compiler optimization
    if is_release {
        cmd.arg("-O");
    }
    // Run `rustc`, and panic if it fails.
    let out = cmd.output().expect("Could not start the Rust compiler");
    if !out.status.success() {
        let stderr = String::from_utf8_lossy(&out.stderr).to_string();
        let stdout = String::from_utf8_lossy(&out.stdout).to_string();
        panic!("Rust compiler error. Command:\n{cmd:?}\nstderr:{stderr}\nstdout:{stdout}");
    }
    out_path
}
const NATIVE_LIB_EXTENSION: &str = "a";
const NATIVE_EXEC_EXTENSION: &str = "elf";
