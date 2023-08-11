#[test]
fn tests() {
    let t = trybuild::TestCases::new();

    t.pass("tests/to_build/functions/bool_type.rs");
    t.pass("tests/to_build/functions/int_type.rs");
    t.pass("tests/to_build/functions/float_type.rs");
    t.pass("tests/to_build/functions/str_type.rs");

    t.pass("tests/to_build/functions/result/bool_type.rs");
    t.pass("tests/to_build/functions/result/int_type.rs");
    t.pass("tests/to_build/functions/result/float_type.rs");
    t.pass("tests/to_build/functions/result/str_type.rs");
}
