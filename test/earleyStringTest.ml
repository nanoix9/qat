open OUnit2

(*
let assert_parse c expect str =
    assert_equal ~ctxt:c ~printer:str_of_expr expect (parse str)
    *)

let foo c = ();;

let suite =
    "earleyString" >::: [
        "test_foo" >:: foo;
    ]
;;


