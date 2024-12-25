mod utilis;
mod add{ mod debug{
  #[cfg(test)]
  const IS_RELEASE:bool = false;
  #[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/add.rs"), true, IS_RELEASE);}}
 mod release{
  #[cfg(test)]
  const IS_RELEASE:bool = true;
  #[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/add.rs"), true, IS_RELEASE);}}}
mod fuzz0{ mod debug{
  #[cfg(test)]
  const IS_RELEASE:bool = false;
  #[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/fuzz0.rs"), true, IS_RELEASE);}}
 mod release{
  #[cfg(test)]
  const IS_RELEASE:bool = true;
  #[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/fuzz0.rs"), true, IS_RELEASE);}}}
mod statics{ mod debug{
  #[cfg(test)]
  const IS_RELEASE:bool = false;
  #[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/statics.rs"), true, IS_RELEASE);}}
 mod release{
  #[cfg(test)]
  const IS_RELEASE:bool = true;
  #[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/statics.rs"), true, IS_RELEASE);}}}
mod btree{ mod debug{
  #[cfg(test)]
  const IS_RELEASE:bool = false;
  #[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/btree.rs"), true, IS_RELEASE);}}
 mod release{
  #[cfg(test)]
  const IS_RELEASE:bool = true;
  #[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/btree.rs"), true, IS_RELEASE);}}}
mod raw_waker{ mod debug{
  #[cfg(test)]
  const IS_RELEASE:bool = false;
  #[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/raw_waker.rs"), true, IS_RELEASE);}}
 mod release{
  #[cfg(test)]
  const IS_RELEASE:bool = true;
  #[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/raw_waker.rs"), true, IS_RELEASE);}}}
mod generics{ mod debug{
  #[cfg(test)]
  const IS_RELEASE:bool = false;
  #[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/generics.rs"), true, IS_RELEASE);}}
 mod release{
  #[cfg(test)]
  const IS_RELEASE:bool = true;
  #[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/generics.rs"), true, IS_RELEASE);}}}
mod pass_ind{ mod debug{
  #[cfg(test)]
  const IS_RELEASE:bool = false;
  #[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/pass_ind.rs"), true, IS_RELEASE);}}
 mod release{
  #[cfg(test)]
  const IS_RELEASE:bool = true;
  #[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/pass_ind.rs"), true, IS_RELEASE);}}}
mod gimili{ mod debug{
  #[cfg(test)]
  const IS_RELEASE:bool = false;
  #[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/gimili.rs"), true, IS_RELEASE);}}
 mod release{
  #[cfg(test)]
  const IS_RELEASE:bool = true;
  #[test]
  fn stable(){crate::test::utilis::compile_file(std::path::Path::new("tests/gimili.rs"), true, IS_RELEASE);}}}
