mod utilis;
mod add {
    mod debug {
        #[cfg(test)]
        const IS_RELEASE: bool = false;
        #[test]
        fn stable() {
            crate::test::utilis::compile_file(
                std::path::Path::new("tests/add.rs"),
                true,
                IS_RELEASE,
            );
        }
    }
    mod release {
        #[cfg(test)]
        const IS_RELEASE: bool = true;
        #[test]
        fn stable() {
            crate::test::utilis::compile_file(
                std::path::Path::new("tests/add.rs"),
                true,
                IS_RELEASE,
            );
        }
    }
}
mod fuzz0 {
    mod debug {
        #[cfg(test)]
        const IS_RELEASE: bool = false;
        #[test]
        fn stable() {
            let executable = crate::test::utilis::compile_file(
                std::path::Path::new("tests/fuzz0.rs"),
                false,
                IS_RELEASE,
            );
            crate::test::utilis::run_test(&executable);
        }
    }
    mod release {
        #[cfg(test)]
        const IS_RELEASE: bool = true;
        #[test]
        fn stable() {
            let executable = crate::test::utilis::compile_file(
                std::path::Path::new("tests/fuzz0.rs"),
                false,
                IS_RELEASE,
            );
            crate::test::utilis::run_test(&executable);
        }
    }
}
mod statics {
    mod debug {
        #[cfg(test)]
        const IS_RELEASE: bool = false;
        #[test]
        fn stable() {
            let executable = crate::test::utilis::compile_file(
                std::path::Path::new("tests/statics.rs"),
                false,
                IS_RELEASE,
            );
            crate::test::utilis::run_test(&executable);
        }
    }
    mod release {
        #[cfg(test)]
        const IS_RELEASE: bool = true;
        #[test]
        fn stable() {
            let executable = crate::test::utilis::compile_file(
                std::path::Path::new("tests/statics.rs"),
                false,
                IS_RELEASE,
            );
            crate::test::utilis::run_test(&executable);
        }
    }
}
mod btree {
    mod debug {
        #[cfg(test)]
        const IS_RELEASE: bool = false;
        #[test]
        fn stable() {
            crate::test::utilis::compile_file(
                std::path::Path::new("tests/btree.rs"),
                true,
                IS_RELEASE,
            );
        }
    }
    mod release {
        #[cfg(test)]
        const IS_RELEASE: bool = true;
        #[test]
        fn stable() {
            crate::test::utilis::compile_file(
                std::path::Path::new("tests/btree.rs"),
                true,
                IS_RELEASE,
            );
        }
    }
}
