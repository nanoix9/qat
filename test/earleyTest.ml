open OUnit2
open Earley


(*
let assert_parse c expect str =
    assert_equal ~ctxt:c ~printer:str_of_expr expect (parse str)
    *)

let const value = t (fun x -> value);;
let literal name = t (fun x -> x = name);;

let gram = g "E"
    [|
        r "E" [| literal "1" |];
        r "E" [| n"E"; literal "+"; n"E"; |];
    |]
;;

let input_string s =
    let x = Array.of_list (Str.split (Str.regexp " +") s) in
    let f i = if i >= Array.length x then
            ""
        else
            Array.get x i
    in
    f
;;

let foo s = Printf.printf "%s\n" (str_of_grammar gram)

let foo2 c =
    let items = earley_match gram (input_string "1 + 1") in
    Printf.printf "%d\n" (DA.length items);
    Printf.printf "%s\n" (str_of_items gram items)
;;

let suite =
    "earley" >::: [
        "test_foo" >:: (fun c -> foo c);
        "test_foo2" >:: (fun c -> foo2 c);
    ]

