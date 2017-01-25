open OUnit2
open Expand


let assert_mmatch c expect pattern expr_ =
    assert_equal ~ctxt:c ~printer:str_of_mmatch_result
                 ~cmp:equal_mmatch_result
                 expect (mmatch pattern expr_)
;;

let atom_id s = Expr.Atom (Expr.Id s);;
let atom_op s = Expr.Atom (Expr.Op s);;
let atom_int i = Expr.Atom (Expr.Imm (Expr.Int i))
let expr_list lst = Expr.ExprList lst;;
let atom_literal lit = Expr.Atom (Literal (Expr.Id lit));;
let atom_var var = Expr.Atom (Variable var);;

let suite =
    "expand" >::: [
        "test_match_atom" >:: (fun c -> assert_mmatch c
                (true, None)
                (atom_literal "foo")
                (atom_id "foo")
            );

        "test_match_var" >:: (fun c -> let e = atom_id "bar" in
                assert_mmatch c
                    (true, Some (StrMap.add "foo" e StrMap.empty))
                    (atom_var "foo") e
            );

        "test_match_list" >:: (fun c ->
                let cond_expr = expr_list [atom_id "x"; atom_op "==";
                        atom_int 0] in
                let blk_true = atom_id "foo" in
                let blk_false = atom_id "bar" in
                assert_mmatch c
                    (true, Some (StrMap.empty |> StrMap.add "cond" cond_expr
                        |> StrMap.add "blk_true" blk_true
                        |> StrMap.add "blk_false" blk_false))
                    (expr_list [atom_literal "if"; atom_var "cond";
                        atom_var "blk_true"; atom_literal "else";
                        atom_var "blk_false"])
                    (expr_list [atom_id "if"; cond_expr; blk_true;
                        atom_id "else"; blk_false])
            );

    ]

