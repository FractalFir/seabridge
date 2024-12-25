use std::{os::unix::process::CommandExt, path::PathBuf, process::Command};

struct CargoTask{
    out_dir:PathBuf,
    crate_dir:PathBuf,
}
type BuildError = ();
impl CargoTask{
  
    pub fn new(out_dir: impl Into<PathBuf>, crate_dir: impl Into<PathBuf>) -> Self {
        Self { out_dir:out_dir.into(), crate_dir:crate_dir.into() }
    }
    
    fn crate_target(&self)->PathBuf{
        let mut target = self.crate_dir.clone();
        target.push("target");
        target
    }
    fn header_target(&self) -> PathBuf{
        let mut target = self.crate_target();
        target.push("header_target");
        target
    }
    pub fn build_headers(&self)->Result<(),BuildError>{
        let header_target = self.header_target();
        let mut cmd = Command::new("cargo");
        cmd.env("CARGO_TARGET_DIR", header_target);
        cmd.arg("build");
        let backend = std::path::absolute(std::env::current_exe().unwrap().with_file_name("librustc_codegen_c").with_extension("so")).unwrap().to_str().unwrap().to_string();
        let packer = std::env::current_exe().unwrap().with_file_name("packer");
        let flags = format!("-Z codegen-backend={backend} -Zcross-crate-inline-threshold=0 -Zinline-mir=no");
        cmd.env("RUSTFLAGS",flags );
        let out = cmd.output().unwrap();
        panic!("cmd:{cmd:?} out:{out:?}");
    }
}
fn main() {
   let out_dir = std::env::args().nth(1).unwrap();
   let crate_dir = std::env::args().nth(2).unwrap_or_else(||std::env::current_dir().unwrap().to_string_lossy().to_string());
   let task = CargoTask::new(out_dir, crate_dir);
   task.build_headers().unwrap();
}
