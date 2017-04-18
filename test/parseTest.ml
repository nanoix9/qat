open OUnit2
open Ast
open Parse

let assert_parse c expect str =
    assert_equal ~ctxt:c ~printer:str_of_ast expect (parse str)
;;

let ls x = NodeList x;;

(* Name the test cases and group them together *)
let suite =
    "parse" >::: [
        "test_id" >:: (fun c -> assert_parse c (Atom (Id "foo")) "foo");
        "test_id_no_space" >:: (fun c -> assert_parse c (Atom (Id "foo_")) "foo_^&**");

        "test_op" >:: (fun c -> assert_parse c (Atom (Op "+-^")) "+-^");

        "test_group_paren" >:: (fun c -> assert_parse c
            (ls [Atom (Id "a"); Atom (Op "+"); Atom (Id "b")]) "(a+b)");

        "test_seq" >:: (fun c -> assert_parse c
            (ls [Atom (Id "do");
                ls [Atom (Id "x"); Atom (Op "-"); Atom (Id "y")]])
            "(x - y;)");

        "test_seq_long" >:: (fun c -> assert_parse c
            (ls [Atom (Id "do");
                ls [Atom (Id "x"); Atom (Op "-");
                    Atom (Id "y"); Atom (Op "*"); Atom (Id "zz");
                    Atom (Op "%"); Atom (Id "u"); Atom (Op "<<");
                    Atom (Imm (Int 2))]])
            "(x - y * zz % u << 2;)");

        "test_mixfix" >:: (fun c -> assert_parse c
            (ls [Atom (Id "if"); Atom (Id "cond"); Atom (Id "then");
                ls [Atom (Op "{}"); Atom (Id "do");
                    ls [Atom (Id "block"); Atom (Id "for")];
                    ls [Atom (Id "_true")]];
                Atom (Id "else");
                ls [Atom (Op "{}"); Atom (Id "do");
                    ls [Atom (Id "block")];
                    ls [Atom (Id "for_false")]]])
            "(if cond then { block for; _true;} else { block; for_false;})");

        "test_nested" >:: (fun c -> assert_parse c
            (ls [Atom (Id "if"); Atom (Id "x");
                Atom (Op "=="); Atom (Imm (Str_ "foo"));
                ls [Atom (Id "if"); Atom (Id "bar");
                    Atom (Op "=="); Atom (Imm (Int 2));
                    ls [Atom (Op "{}"); Atom (Id "do");
                        ls [Atom (Id "if"); Atom (Id "foobar");
                            Atom (Op "&&"); Atom (Id "barfoo");
                            ls [Atom (Op "{}"); Atom (Id "do");
                                ls [Atom (Id "do"); Atom (Id "it")]]]]];
            Atom (Id "else");
            ls [Atom (Op "{}"); Atom (Id "do");
                ls [Atom (Id "do"); Atom (Id "else")]]])
            "(if x == \"foo\" (
                if bar == 2 {
                    if foobar && barfoo {
                        do it;
                    };
                }
            ) else {
                do else;
            })");

    ]


