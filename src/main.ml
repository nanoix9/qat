open Expr
open Parse
open Earley
open Expand2

(*=========== test parse ============*)
let parse = Parse.parse;;

let parse_main () =
    let argv   = Array.to_list Sys.argv in
    let args   = List.tl argv in
    let expr   = "(" ^ String.concat " " args ^ ")" in
    let result = parse expr in
        Printf.printf "%s: %s\n" expr (unparse result)
;;

(*=========== test Earley ============*)
let parse = Earley.parse;;

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

let earley_foo s = Printf.printf "%s\n" (str_of_grammar gram)

let earley_foo2 c =
    let input = input_string "1 + 1" in
    let items = earley_match gram input in
    Printf.printf "input: %s\n" (Util.joina ~sep:" • " input);
    Printf.printf "%d\n" (DA.length items);
    Printf.printf "%s\n" (str_of_items (fun x -> x) gram items)
;;

let earley_foo3 c = Printf.printf "%s\n" (string_of_bool
        (recognize gram (input_string "1 + 1")))

let earley_foo4 c = Printf.printf "%s\n" (
    match parse gram (input_string "1 + 1") with
    | None -> "ERROR"
    | Some t -> str_of_parse_tree (fun x -> x) t)

let earley_test_ss_defect c =
    let input = input_string "x x x" in
    let items = earley_match gram2 input in
    Printf.printf "input: %s\n" (Util.joina ~sep:" • " input);
    Printf.printf "%d\n" (DA.length items);
    Printf.printf "%s\n" (str_of_items (fun x -> x) gram2 items)
;;

let earley_string_main () =
    earley_foo ();
    earley_foo2 ();
    earley_foo3 ();
    earley_foo4 ();
    earley_test_ss_defect ()
;;

(*========== test macro expanding ============*)
let mmngr =
    let m = create_macro_manager () in
    add_macro m (new_macro [v"x"; lo"+"; v"y"] [ls"add"; v"x"; v"y"]);
    add_macro m (new_macro [v"x"; lo"-"; v"y"] [ls"sub"; v"x"; v"y"]);
    build_grammar m;
    m
;;

let e lst :expr = ExprList lst;;
let id s :expr = Atom (Id s);;
let op s :expr = Atom (Op s);;
let i n :expr = Atom (Imm (Int n));;

let macro_main s = Util.println (show_macro_manager mmngr);
    let pt = parse_pattern mmngr (e [id "foo"; op "+"; id "bar"; op "-"; i 10]) in
    Util.println (Earley.str_of_parse_tree str_of_expr pt)
;;

(*=========== main ===========*)
let main () =
    ();
    (*parse_main ();*)

    (*earley_string_main ();*)

    macro_main ()
;;

let () = main ();;
