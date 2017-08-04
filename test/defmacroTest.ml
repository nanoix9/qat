open OUnit2
open Expand
open Ast
open Macro
(*open Earley*)

(*let assert_expand_parse c expect exp =*)
    (*let m = create_macro_manager () in*)
    (*(*Util.println (show_macro_manager m);*)*)
    (*assert_equal ~ctxt:c*)
        (*~cmp:eq_ast*)
        (*~printer:(str_of_ast)*)
        (*expect (expand m exp)*)
(*;;*)

(*let assert_expand_parse_error exp err =*)
    (*assert_raises ~msg:err (Macro.MacroErr err)*)
        (*(fun () ->*)
            (*let m = create_macro_manager () in*)
            (*(*Util.println (show_macro_manager m);*)*)
            (*parse_pattern m exp)*)
(*;;*)

let assert_expand c expect exp =
    let m = create_macro_manager () in
    (*Util.println (show_macro_manager m);*)
    assert_equal ~ctxt:c
        ~printer:str_of_ast
        expect (expand m exp)
;;


let e lst :ast = NodeList lst;;
let id s :ast = Atom (Id s);;
let op s :ast = Atom (Op s);;
let i n :ast = Atom (Imm (Int (string_of_int n)));;

(*let madd = new_macro (Infix Left) [x; lo"+"; y] [ls"add"; x; y]*)
(*let msub = new_macro (Infix Left) [x; lo"-"; y] [ls"sub"; x; y]*)
(*let mmul = new_macro (Infix Left) [x; lo"*"; y] [ls"mul"; x; y]*)
(*let mdiv = new_macro (Infix Right) [x; lo"/"; y] [ls"slash"; x; y]*)
(*let mpow = new_macro (Infix Right) [x; lo"**"; y] [ls"pow"; x; y]*)
(*let mnot = new_macro Prefix [lo"!"; x] [ls"not"; x]*)
(*let mge = new_macro (Infix Non) [x; lo">="; y] [ls"ge"; x; y]*)
(*let mw = new_macro Closed [ls"while"; x; ls"do"; y; ls"done"]*)
                    (*[ls"while_loop"; x; y]*)
(*let mif = new_macro Prefix [ls"if"; x; ls"then"; y]*)
                    (*[ls"if_then"; x; y]*)
(*let mif2 = new_macro Prefix [ls"if"; x; ls"then"; y; ls"else"; z]*)
                    (*[ls"if_else"; x; y; z]*)
(*let mter = new_macro (Infix Right) [x; lo"?"; y; lo":"; z]*)
                    (*[ls"if_else"; x; y; z]*)
(*let minc = new_macro Postfix [x; lo"++"] [ls"inc"; x]*)
(*let mdec = new_macro Postfix [x; lo"--"] [ls"dec"; x]*)
(*;;*)

let add_macro = Expand.add_macro;;

let ph = id"_";;
let x = id"x"
let y = id"y"
let z = id"z"
let q = op"?"
;;

let suite =
    "defmacro" >::: [
        "test_expand_right" >:: (fun c ->
            assert_expand c
                (e [id"do";
                    id"nil"; id"nil"; id"nil"; id"nil";
                    e [op"-";
                        e [op"+";
                            x;
                            e [op"/"; e [op"*"; y; z]; id"u"]];
                        id"v"]])
                (e [id"do";
                    e [id"defmacro";
                        e [q; x; op"+"; q; y];
                        e [id"associativity"; id"left"];
                        e [id"precedence"; e []];
                        e [op"+"; q; x; q; y];
                    ];
                    e [id"defmacro";
                        e [q; x; op"-"; q; y];
                        e [id"associativity"; id"left"];
                        e [id"precedence"; id"equals"; e [ph; op"+"; ph]];
                        e [op"-"; q; x; q; y];
                    ];
                    e [id"defmacro";
                        e [q; x; op"*"; q; y];
                        e [id"associativity"; id"left"];
                        e [id"precedence"; id"higherThan"; e [ph; op"+"; ph]];
                        e [op"*"; q; x; q; y];
                    ];
                    e [id"defmacro";
                        e [q; x; op"/"; q; y];
                        e [id"associativity"; id"left"];
                        e [id"precedence"; id"equals"; e [ph; op"*"; ph]];
                        e [op"/"; q; x; q; y];
                    ];
                    e [x; op"+"; y; op"*"; z; op"/"; id"u"; op"-"; id"v";];
                    ])
            );

    ]
;;


