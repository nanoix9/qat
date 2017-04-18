open OUnit2

let foo c = ();;

let suite =
    "earleyString" >::: [
        "test_foo" >:: foo;
    ]
;;


