mod utilis;
mod fuzz0{
 #[cfg(test)]
    static COMPILE_LOCK:std::sync::Mutex<()> = std::sync::Mutex::new(());
mod debug{

#[cfg(test)]
  const IS_RELEASE:bool = false;
 
#[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/fuzz0.rs"), true, IS_RELEASE);}}
 mod release{
  #[cfg(test)]
  const IS_RELEASE:bool = true;
  #[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/fuzz0.rs"), true, IS_RELEASE);}}}
mod btree{
 #[cfg(test)]
    static COMPILE_LOCK:std::sync::Mutex<()> = std::sync::Mutex::new(());
mod debug{

#[cfg(test)]
  const IS_RELEASE:bool = false;
 
#[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/btree.rs"), true, IS_RELEASE);}}
 mod release{
  #[cfg(test)]
  const IS_RELEASE:bool = true;
  #[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/btree.rs"), true, IS_RELEASE);}}}
mod raw_waker{
 #[cfg(test)]
    static COMPILE_LOCK:std::sync::Mutex<()> = std::sync::Mutex::new(());
mod debug{

#[cfg(test)]
  const IS_RELEASE:bool = false;
 
#[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/raw_waker.rs"), true, IS_RELEASE);}}
 mod release{
  #[cfg(test)]
  const IS_RELEASE:bool = true;
  #[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/raw_waker.rs"), true, IS_RELEASE);}}}
mod generics{
 #[cfg(test)]
    static COMPILE_LOCK:std::sync::Mutex<()> = std::sync::Mutex::new(());
mod debug{

#[cfg(test)]
  const IS_RELEASE:bool = false;
 
#[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/generics.rs"), true, IS_RELEASE);}}
 mod release{
  #[cfg(test)]
  const IS_RELEASE:bool = true;
  #[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/generics.rs"), true, IS_RELEASE);}}}
mod pass_ind{
 #[cfg(test)]
    static COMPILE_LOCK:std::sync::Mutex<()> = std::sync::Mutex::new(());
mod debug{

#[cfg(test)]
  const IS_RELEASE:bool = false;
 
#[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/pass_ind.rs"), true, IS_RELEASE);}}
 mod release{
  #[cfg(test)]
  const IS_RELEASE:bool = true;
  #[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/pass_ind.rs"), true, IS_RELEASE);}}}
mod gimili{
 #[cfg(test)]
    static COMPILE_LOCK:std::sync::Mutex<()> = std::sync::Mutex::new(());
mod debug{

#[cfg(test)]
  const IS_RELEASE:bool = false;
 
#[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/gimili.rs"), true, IS_RELEASE);}}
 mod release{
  #[cfg(test)]
  const IS_RELEASE:bool = true;
  #[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/gimili.rs"), true, IS_RELEASE);}}}
mod add{
 #[cfg(test)]
    static COMPILE_LOCK:std::sync::Mutex<()> = std::sync::Mutex::new(());
mod debug{

#[cfg(test)]
  const IS_RELEASE:bool = false;
 
#[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/add.rs"), true, IS_RELEASE);}}
 mod release{
  #[cfg(test)]
  const IS_RELEASE:bool = true;
  #[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/add.rs"), true, IS_RELEASE);}}}
mod statics{
 #[cfg(test)]
    static COMPILE_LOCK:std::sync::Mutex<()> = std::sync::Mutex::new(());
mod debug{

#[cfg(test)]
  const IS_RELEASE:bool = false;
 
#[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/statics.rs"), true, IS_RELEASE);}}
 mod release{
  #[cfg(test)]
  const IS_RELEASE:bool = true;
  #[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/statics.rs"), true, IS_RELEASE);}}}
