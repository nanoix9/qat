open OUnit2
open Expand2
open Expr

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

let foo s = Util.println (show_macro_manager mmngr);
    let pt = parse_pattern mmngr (e [id "foo"; op "+"; id "bar"; op "-"; i 10]) in
    Util.println (Earley.str_of_parse_tree str_of_expr pt)
;;

let suite =
    "expand2" >::: [
        "test_foo" >:: foo;
    ]
;;


