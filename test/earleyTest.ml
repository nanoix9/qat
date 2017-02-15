open OUnit2
open Earley


(*
let assert_parse c expect str =
    assert_equal ~ctxt:c ~printer:str_of_expr expect (parse str)
    *)

let gram = g "E"
    [|
        r "E" [| t (fun x -> true) |];
    |]
;;

let input x =
    let f i = Array.get x i in f
;;

let foo s = Printf.printf "%s\n" (str_of_grammar gram)

let foo2 c =
    let items = earley_match gram (input [| 1 |]) in
    Printf.printf "%d\n" (DA.length items);
    Printf.printf "%s\n" (str_of_items gram items)
;;

let suite =
    "earley" >::: [
        "test_foo" >:: (fun c -> foo c);
        "test_foo2" >:: (fun c -> foo2 c);
    ]

