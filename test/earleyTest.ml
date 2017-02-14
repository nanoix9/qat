open OUnit2
open Earley


let foo s = Printf.printf "%s\n" (str_of_grammar
    (g (n "E")
    [|
        r (n "E") [| t (fun x -> true) |];
    |]))
;;


let suite =
    "earley" >::: [
        "test_foo" >:: (fun c -> foo c);
    ]

