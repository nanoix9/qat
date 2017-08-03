open OUnit2
open Expand
open Ast
open Macro
open Earley

let rec parse_tree_equals p q :bool =
    match p, q with
    | Leaf f, Leaf g when f = g -> true
    | Tree (_, t), Tree (_, s) -> parse_tree_array_equals t s 0
    | _ -> false
and parse_tree_array_equals t s i :bool =
    let tlen = Array.length t in
    let slen = Array.length s in
    if tlen <> slen then false
    else if i = tlen then true
    else parse_tree_equals (Array.get t i) (Array.get s i)
        && parse_tree_array_equals t s (i+1)
;;

let assert_expand_parse c setup expect exp =
    let m = create_macro_manager () in
    setup m;
    build_grammar m;
    (*Util.println (show_macro_manager m);*)
    assert_equal ~ctxt:c
        ~cmp:parse_tree_equals
        ~printer:(str_of_parse_tree str_of_ast)
        expect (parse_pattern m exp)
;;

let assert_expand_parse_error setup exp err =
    assert_raises ~msg:err (Macro.MacroErr err)
        (fun () ->
            let m = create_macro_manager () in
            setup m;
            build_grammar m;
            (*Util.println (show_macro_manager m);*)
            parse_pattern m exp)
;;

let assert_expand c setup expect exp =
    let m = create_macro_manager () in
    setup m;
    build_grammar m;
    (*Util.println (show_macro_manager m);*)
    assert_equal ~ctxt:c
        ~printer:str_of_ast
        expect (expand m exp)
;;


let e lst :ast = NodeList lst;;
let id s :ast = Atom (Id s);;
let op s :ast = Atom (Op s);;
let i n :ast = Atom (Imm (Int (string_of_int n)));;

let lf_id s = Leaf (id s);;
let lf_op s = Leaf (op s);;
let lf_i n = Leaf (i n);;
let tr arr = Tree (0, arr);;

let x = v"x"
let y = v"y"
let z = v"z"
;;

let madd = new_macro_util (Infix Left) [x; lo"+"; y] [ls"add"; x; y]
let msub = new_macro_util (Infix Left) [x; lo"-"; y] [ls"sub"; x; y]
let mmul = new_macro_util (Infix Left) [x; lo"*"; y] [ls"mul"; x; y]
let mdiv = new_macro_util (Infix Right) [x; lo"/"; y] [ls"slash"; x; y]
let mpow = new_macro_util (Infix Right) [x; lo"**"; y] [ls"pow"; x; y]
let mnot = new_macro_util Prefix [lo"!"; x] [ls"not"; x]
let mge = new_macro_util (Infix Non) [x; lo">="; y] [ls"ge"; x; y]
let mw = new_macro_util Closed [ls"while"; x; ls"do"; y; ls"done"]
                    [ls"while_loop"; x; y]
let mif = new_macro_util Prefix [ls"if"; x; ls"then"; y]
                    [ls"if_then"; x; y]
let mif2 = new_macro_util Prefix [ls"if"; x; ls"then"; y; ls"else"; z]
                    [ls"if_else"; x; y; z]
let mter = new_macro_util (Infix Right) [x; lo"?"; y; lo":"; z]
                    [ls"if_else"; x; y; z]
let minc = new_macro_util Postfix [x; lo"++"] [ls"inc"; x]
let mdec = new_macro_util Postfix [x; lo"--"] [ls"dec"; x]
;;
    (*add_macro_between m m4 (Some m3.id) None;*)
    (*add_macro_between m m4 (Some m1.id) (Some m3.id);*)

let add_macro_between = Expand.add_macro_between;;
let add_macro_equals = Expand.add_macro_equals;;

let suite =
    "expand" >::: [
        "test_parse_left" >:: (fun c ->
            let f m =
                add_macro_between m mmul None None;
                add_macro_between m madd (Some mmul.id) None;
                add_macro_equals m msub madd.id;
                (*add_macro_between m m5 None (Some m3.id);*)
            in
            assert_expand_parse c f
                (tr [| tr [| lf_id "foo"; lf_op "+"; lf_id"bar" |];
                    lf_op"-"; lf_i 10 |])
                (e [id "foo"; op "+"; id "bar"; op "-"; i 10])
            );

        "test_parse_right" >:: (fun c ->
            let f m =
                add_macro_between m mmul None None;
                add_macro_between m madd (Some mmul.id) None;
                add_macro_equals m msub madd.id;
                add_macro_between m mpow None (Some mmul.id);
            in
            assert_expand_parse c f
                (tr [| lf_id"a"; lf_op"+";
                    tr [| lf_id"b"; lf_op"*";
                        tr [| lf_id"c"; lf_op"**";
                            tr [| lf_id"d"; lf_op"**"; lf_id"e" |] |] |] |] )
                (e [id "a"; op "+"; id "b"; op "*"; id "c";
                    op"**"; id"d"; op"**"; id"e";])
            );

        "test_parse_prefix" >:: (fun c ->
            let f m =
                add_macro_between m mge None None;
                add_macro_between m mnot (Some mge.id) None;
            in
            assert_expand_parse c f
                (tr [| lf_op"!";
                    tr [| lf_op"!";
                        tr [| lf_id"x"; lf_op">="; lf_id"y" |] |] |])
                (e [op"!"; op"!"; id"x"; op">="; id"y"])
            );

        "test_parse_postfix" >:: (fun c ->
            let f m =
                add_macro_between m minc None None;
                add_macro_equals m mdec minc.id;
            in
            assert_expand_parse c f
                (tr [| tr [| lf_id"x"; lf_op"++"; |]; lf_op"--" |])
                (e [id"x"; op"++"; op"--";])
            );

        "test_parse_closed" >:: (fun c ->
            let f m =
                add_macro_between m mge None None;
                add_macro_between m mdec None None;
                add_macro_between m mw None None;
                (*add_macro_between m m5 None (Some m3.id);*)
            in
            assert_expand_parse c f
                (tr [| lf_id"while";
                    tr [| lf_id "x"; lf_op ">="; lf_i 0 |];
                    lf_id"do";
                    tr [| lf_id"x"; lf_op"--" |];
                    lf_id "done" |])
                (e [id"while"; id "x"; op ">="; i 0; id"do";
                    id"x"; op"--"; id"done"])
            );

        "test_parse_if" >:: (fun c ->
            let f m =
                add_macro_between m mif None None;
                add_macro_between m mge (Some mif.id) None;
                add_macro_between m madd None (Some mif.id);
            in
            assert_expand_parse c f
                (tr [| lf_id"if";
                    tr [| lf_id"x"; lf_op">="; lf_i 10 |];
                    lf_id"then";
                    tr [| lf_id"if"; lf_id"y"; lf_id"then";
                        tr [| lf_id"x"; lf_op"+"; lf_id"y" |] |] |])
                (e [id"if"; id"x"; op">="; i 10; id"then";
                    id"if"; id"y"; id"then"; id"x"; op"+"; id"y"])
            );

        "test_parse_no_relation" >:: (fun c ->
            let f m =
                add_macro_between m madd None None;
                add_macro_between m msub None None;
            in
            assert_expand_parse_error f
                (e [id "x"; op "-"; id "y"; op "+"; id"z"])
                "macro expanding error at token 4: [OP(+), ID(z)]"
            );

        "test_parse_add_between_no_relation" >:: (fun c ->
            let f m =
                add_macro_between m madd None None;
                add_macro_between m msub None None;
                add_macro_between m mmul (Some madd.id) (Some msub.id);
            in
            assert_expand_parse_error f (e [])
                "Add Precedence between group 1 and 2 but 1 is not higher than 2"
            );

        "test_parse_add_between_lower" >:: (fun c ->
            let f m =
                add_macro_between m mmul None None;
                add_macro_between m madd (Some mmul.id) None;
                add_macro_between m msub (Some madd.id) (Some mmul.id);
            in
            assert_expand_parse_error f (e [])
                "Add Precedence between group 2 and 1 but 2 is not higher than 1"
            );

        "test_expand_right" >:: (fun c ->
            let f m =
                add_macro_between m mw None None;
                add_macro_equals m mif mw.id;
                add_macro_equals m mif2 mw.id;
                add_macro_between m minc (Some mw.id) None;
                add_macro_equals m mdec minc.id;
                add_macro_between m mpow (Some minc.id) None;
                add_macro_between m mmul (Some mpow.id) None;
                add_macro_between m madd (Some mmul.id) None;
                add_macro_equals m msub madd.id;
                add_macro_between m mge (Some msub.id) None;
            in
            assert_expand c f
                (e [id"while_loop";
                    e [id"ge"; id"x"; i 0];
                    e [id"if_else";
                        e [id"ge"; id"foo"; i 10];
                        e [id"mul"; id"foo"; id"x"];
                        e [id"dec"; id"x"]]])
                (e [id"while"; id "x"; op ">="; i 0; id"do";
                        id"if"; id"foo"; op">="; i 10; id"then";
                            id"foo"; op"*"; id"x";
                        id"else";
                            e [id"x"; op"--"];
                    id"done"])
            );

    ]
;;


