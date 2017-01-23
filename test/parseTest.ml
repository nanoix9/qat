open OUnit2
open Expr
open Parse

let assert_parse c expect str =
    assert_equal ~ctxt:c ~printer:str_of_expr expect (parse str)

(* Name the test cases and group them together *)
let suite =
    "suite" >::: [
        "test_id" >:: (fun c -> assert_parse c (Atom (Id "foo")) "foo");
        "test_id_no_space" >:: (fun c -> assert_parse c (Atom (Id "foo_")) "foo_^&**");

        "test_op" >:: (fun c -> assert_parse c (Atom (Op "+-^")) "+-^");

        "test_group_paren" >:: (fun c -> assert_parse c
            (ExprList [Atom (Id "a"); Atom (Op "+"); Atom (Id "b")]) "(a+b)");

        "test_seq" >:: (fun c -> assert_parse c
            (ExprList [ExprList [Atom (Id "x"); Atom (Op "-"); Atom (Id "y")]]) "(x - y;)");

        "test_seq_long" >:: (fun c -> assert_parse c
            (ExprList [ExprList [Atom (Id "x"); Atom (Op "-"); Atom (Id "y");
                Atom (Op "*"); Atom (Id "zz"); Atom (Op "%"); Atom (Id "u");
                Atom (Op "<<"); Atom (Imm (Int 2))]])
            "(x - y * zz % u << 2;)");

        "test_mixfix" >:: (fun c -> assert_parse c
            (ExprList [Atom (Id "if"); Atom (Id "cond"); Atom (Id "then");
                ExprList [Atom (Op "{}");
                    ExprList [Atom (Id "block"); Atom (Id "for")];
                    ExprList [Atom (Id "_true")]];
                Atom (Id "else");
                ExprList [Atom (Op "{}");
                    ExprList [Atom (Id "block")];
                    ExprList [Atom (Id "for_false")]]])
            "(if cond then { block for; _true;} else { block; for_false;})");

        "test_nested" >:: (fun c -> assert_parse c
            (ExprList [Atom (Id "if"); Atom (Id "x");
                Atom (Op "=="); Atom (Imm (Str_ "foo"));
                ExprList [Atom (Id "if"); Atom (Id "bar");
                    Atom (Op "=="); Atom (Imm (Int 2));
                    ExprList [Atom (Op "{}");
                        ExprList [Atom (Id "if"); Atom (Id "foobar");
                            Atom (Op "&&"); Atom (Id "barfoo");
                            ExprList [Atom (Op "{}");
                                ExprList [Atom (Id "do"); Atom (Id "it")]]]]];
            Atom (Id "else");
            ExprList [Atom (Op "{}");
                ExprList [Atom (Id "do"); Atom (Id "else")]]])
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


