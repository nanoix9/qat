open OUnit2
open Expand2
open Expr

let foo c = () ;;

let suite =
    "expand2" >::: [
        "test_foo" >:: foo;
    ]
;;


