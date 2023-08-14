#[test]
fn tests() {
    let t = trybuild::TestCases::new();

    t.pass("tests/to_build/functions/bool_type.rs");
    t.pass("tests/to_build/functions/int_type.rs");
    t.pass("tests/to_build/functions/float_type.rs");
    t.pass("tests/to_build/functions/str_type.rs");
    t.pass("tests/to_build/functions/date_type.rs");
    t.pass("tests/to_build/functions/blob_type.rs");

    t.pass("tests/to_build/functions/result/bool_type.rs");
    t.pass("tests/to_build/functions/result/int_type.rs");
    t.pass("tests/to_build/functions/result/float_type.rs");
    t.pass("tests/to_build/functions/result/str_type.rs");
    t.pass("tests/to_build/functions/result/date_type.rs");
    t.pass("tests/to_build/functions/result/blob_type.rs");

    t.pass("tests/to_build/functions/defaults/bool_type.rs");
    t.pass("tests/to_build/functions/defaults/int_type.rs");
    t.pass("tests/to_build/functions/defaults/float_type.rs");
    t.pass("tests/to_build/functions/defaults/str_type.rs");
    t.compile_fail("tests/to_build/functions/defaults/date_type.rs");
    t.compile_fail("tests/to_build/functions/defaults/blob_type.rs");
}
