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

let gram2 = g "S"
    [|
        r "S" [| literal "x" |];
        r "S" [| n"S"; n"S"; |];
    |]
;;

let input_string s =
    Array.of_list (Str.split (Str.regexp " +") s)
;;

let foo s = Printf.printf "%s\n" (str_of_grammar gram)

let foo2 c =
    let input = input_string "1 + 1" in
    let items = earley_match gram input in
    Printf.printf "input: %s\n" (Util.joina ~sep:" • " input);
    Printf.printf "%d\n" (DA.length items);
    Printf.printf "%s\n" (str_of_items (fun x -> x) gram items)
;;

let foo3 c = Printf.printf "%s\n" (string_of_bool
        (recognize gram (input_string "1 + 1")))

let foo4 c = Printf.printf "%s\n" (
    match parse gram (input_string "1 + 1") with
    | None -> "ERROR"
    | Some t -> str_of_parse_tree (fun x -> x) t)

let test_ss_defect c =
    let input = input_string "x x x" in
    let items = earley_match gram2 input in
    Printf.printf "input: %s\n" (Util.joina ~sep:" • " input);
    Printf.printf "%d\n" (DA.length items);
    Printf.printf "%s\n" (str_of_items (fun x -> x) gram2 items)
;;

let suite =
    "earleyString" >::: [
        "test_foo" >:: foo;
        "test_foo2" >:: foo2;
        "test_foo3" >:: foo3;
        "test_foo4" >:: foo4;

        "test_ss_defect" >:: test_ss_defect;
    ]
;;


