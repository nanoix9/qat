open Printf
open Expr
open Parse
open Earley
open Macro
open Expand
open Env
open Evaluate
open Builtin

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

let const value = t "_" (fun x -> value);;
let literal name = t name (fun x -> x = name);;

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
    Printf.printf "input: %s\n" (Util.joina " • " input);
    Printf.printf "%d\n" (DA.length items);
    Printf.printf "%s\n" (str_of_items (fun x -> x) gram items)
;;

let earley_foo3 c = Printf.printf "%s\n" (string_of_bool
        (recognize gram (input_string "1 + 1")))

let earley_foo4 c = Printf.printf "%s\n" (
    let t = parse gram (input_string "1 + 1") in
    str_of_parse_tree (fun x -> x) t)

let earley_test_ss_defect c =
    let input = input_string "x x x" in
    let items = earley_match gram2 input in
    Printf.printf "input: %s\n" (Util.joina " • " input);
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
    let m1 = new_macro (Infix Left) [v"x"; lo"+"; v"y"]
            [ls"add"; v"x"; v"y"] in
    let m2 = new_macro (Infix Left) [v"x"; lo"-"; v"y"]
            [ls"sub"; v"x"; v"y"] in
    let m3 = new_macro (Infix Left) [v"x"; lo"*"; v"y"]
            [ls"mul"; v"x"; v"y"] in
    let m4 = new_macro (Infix Right) [v"x"; lo"/"; v"y"]
            [ls"slash"; v"x"; v"y"] in
    let m5 = new_macro (Infix Right) [v"x"; lo"**"; v"y"]
            [ls"pow"; v"x"; v"y"] in
    add_macro_between m m3 None None;
    add_macro_between m m1 (Some m3.id) None;
    add_macro_equals m m2 m1.id;
    add_macro_between m m5 None (Some m3.id);
    (*add_macro_between m m4 (Some m3.id) None;*)
    (*add_macro_between m m4 (Some m1.id) (Some m3.id);*)
    build_grammar m;
    m
;;

let e lst :expr = ExprList lst;;
let id s :expr = Atom (Id s);;
let op s :expr = Atom (Op s);;
let i n :expr = Atom (Imm (Int n));;

let show_grammar () = Util.println (show_macro_manager mmngr);;

let test_match () =
    let exp = e [id "foo"; op "+"; id "bar"; op "-"; i 10] in
    (*let exp = e [id "foo"; op "+"; id "bar"; op "@"; i 10] in*)
    let pt = parse_pattern_raw mmngr exp in
    Printf.printf "------- Parsed --------\n%s\n"
            (Earley.str_of_parse_tree str_of_expr pt);
    let spt = simplify_parse_tree mmngr pt in
    Printf.printf "------- Simplified --------\n%s\n"
            (Earley.str_of_parse_tree str_of_expr spt)
;;

let test_substitute () =
    let exp = e [id "foo"; op "+"; id "bar"; op "-"; i 10] in
    (*let exp = e [id "foo"; op "+"; id "bar"; op "@"; i 10] in*)
    (*let pt = parse_pattern_raw mmngr exp in*)
    (*Printf.printf "------- Parsed --------\n%s\n"*)
            (*(Earley.str_of_parse_tree str_of_expr pt);*)
    (*let spt = simplify_parse_tree mmngr pt in*)
    let spt = parse_pattern mmngr exp in
    Printf.printf "------- Matched --------\n%s\n"
            (Earley.str_of_parse_tree str_of_expr spt);
    let eexp = expand_parse_tree mmngr spt in
    Printf.printf "------- Expanded --------\n%s\n"
            (Expr.str_of_expr eexp)
;;

let foobar () =
    let prcdn = make_precedences () in
    let plus = (new_macro (Infix Right)
            [v"x"; lo"+"; v"y"]
            [ls"add"; v"x"; v"y"])
    in
    let sub = (new_macro (Infix Right)
            [v"x"; lo"-"; v"y"]
            [ls"sub"; v"x"; v"y"])
    in
    Macro.add_macro_between prcdn plus None None;
    Macro.add_macro_between prcdn sub (Some plus.id) None;
    printf "%s\n" (str_of_precedences prcdn)
;;

let macro_main s =
    show_grammar ();
    (*test_match ()*)
    test_substitute ()
    (*foobar ()*)
;;

(*========== Graph ==========*)

let print_graph () =
    let strs_of_v v = [string_of_int v; ] in
    let g = G.create () in
    let v0 = G.V.create 0 in
    let v1 = G.V.create 10 in
    let v2 = G.V.create 20 in
    let v3 = G.V.create 30 in
    let v4 = G.V.create 40 in
    let v5 = G.V.create 50 in
    let v6 = G.V.create 60 in
    G.add_vertex g v0;
    G.add_vertex g v1;
    G.add_vertex g v2;

    G.add_edge g v0 v1;
    G.add_edge g v1 v2;
    G.add_edge g v1 v3;
    G.add_edge g v1 v4;
    G.add_edge g v3 v5;
    G.add_edge g v3 v6;
    printf "%s\n" (str_of_dag strs_of_v g)
    (*let plt = make_plotter () in*)
    (*(*expand_canvas_opt plt.canvas 5 5;*)*)
    (*draw_point plt.canvas 2 3 '*';*)
    (*append_sym plt 'a';*)
    (*append_sym plt 'b';*)
    (*append_sym plt 'c';*)
    (*draw_line plt.canvas (8,8) (1,1) 5;*)
    (*draw_line plt.canvas (8,8) (1,0) 5;*)
    (*draw_line plt.canvas (8,8) (0,1) 5;*)
    (*draw_line plt.canvas (8,8) (0,-1) 5;*)
    (*printf "%s\n" (get_content plt.canvas)*)
;;

(*================ Evaluate =================*)
let ee lst :eexpr = ExprList lst;;
let sym s :eexpr = Atom (Sym s);;
let i n :eexpr = Atom (Obj (make_int n));;

let test_eval () =
    (*let exp = e [id "foo"; op "+"; id "bar"; op "-"; i 10] in*)
    let exp = ee [ sym "do";
        ee [sym "def"; sym "x"; i 10];
        sym "x"]
    in
    let res = evaluate exp in
    ()
;;

let eval_main () =
    test_eval ()
;;

(*=========== main ===========*)
let main () =
    ();
    (*parse_main ();*)

    (*earley_string_main ();*)

    (*macro_main ()*)

    eval_main ()

    (*print_graph ()*)
;;

let () = main ();;
