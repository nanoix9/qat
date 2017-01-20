open OUnit2
open Expr
open Lex

let lex_str s = pop_token (new_stream s)
let assert_lex c expect str =
    assert_equal ~ctxt:c ~printer:str_of_token expect (lex_str str)

let test_id c = assert_lex c (Id "foo") "foo"
let test_id_underscore c = assert_lex c (Id "_foo") "_foo"
let test_id_no_space c = assert_lex c (Id "foo") "foo9"

let test_int c = assert_lex c (Imm (Int 2)) "2"
let test_int_neg c = assert_lex c (Imm (Int (-100))) "-100"
let test_int_hex c = assert_lex c (Imm (Int 30)) "0x1E"

let test_str_has_quote c = assert_lex c (Imm (Str_ "a\"b")) "\"a\\\"b\""
let test_str_single c = assert_lex c (Imm (Str_ "foo\"b")) "'foo\\\"b'"

let test_op_single c = assert_lex c (Op "+") "+haha"

(* Name the test cases and group them together *)
let suite =
    "suite" >::: [
        "test_id" >:: test_id;
        "test_id_underscore" >:: test_id_underscore;

        "test_int" >:: test_int;
        "test_int_neg" >:: test_int_neg;
        "test_int_hex" >:: test_int_hex;

        "test_bool_true" >:: (fun c -> assert_lex c (Imm (Bool true)) "true");
        "test_bool_false" >:: (fun c -> assert_lex c (Imm (Bool false)) "false");
        "test_bool_case" >:: (fun c -> assert_lex c (Id "True") "True");

        "test_str_has_quote" >:: test_str_has_quote;
        "test_str_single" >:: test_str_single;

        "test_op_single" >:: test_op_single;
        "test_op_compound" >:: fun c -> assert_lex c (Op "$*$") "$*$ 12";
    ]


