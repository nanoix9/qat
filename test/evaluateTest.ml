open OUnit2
open Ast
open Env
open Evaluate
open Builtin
open Big_int

let assert_eval c expect stmt =
    let evaluator = make_evaluator () in
    let got = evaluate evaluator stmt in
    assert_equal ~ctxt:c
        ~cmp:eq_evalret
        ~printer:str_of_evalret
        expect got
;;

let assert_eval_val c expect stmt =
    assert_eval c (EvalVal expect) stmt
;;

let e lst :ast = NodeList lst;;
let id s :ast = Atom (Id s);;
let op s :ast = Atom (Op s);;
let i n :ast = Atom (Imm (Int (string_of_int n)));;
let b x :ast = Atom (Imm (Bool (string_of_bool x)));;
let s x :ast = Atom (Imm (Str_ x));;

let make_int i = Builtin.make_int (big_int_of_int i);;

(* Name the test cases and group them together *)
let suite =
    "evaluate" >::: [
        "test_sym" >:: (fun c -> assert_eval_val c
            (make_int 10)
            (i 10));

        "test_if" >:: (fun c -> assert_eval_val c
            (make_str "right")
            (e [id"do";
                e [id"def"; id"x"; b true];
                e [id"if"; id"x"; e [id"return"; s"right"]; i 0];
                ]));

        "test_return" >:: (fun c -> assert_eval_val c
            (make_bool true)
            (e [id"do";
                e [id"def"; id"x"; b true];
                e [id"return"; id"x"];
                ]));

        "test_func" >:: (fun c -> assert_eval_val c
            (make_str "good")
            (e [id"do";
                e [id"def"; id"foo";
                    e [id"func"; id"foo";
                        e [id"x"; id"y"];
                        e [id"if"; id"x";
                            e [id"return"; id"y"];
                            e [id"return"; i 100]]]];
                e [id"foo"; b true; s "good"];
                ]));

        "test_type" >:: (fun c -> assert_eval_val c
            (make_type (fullname_of_list ["foo_type"; "__main__"]) obj_o)
            (e [id"type"; id"foo_type"]));

        "test_subtype" >:: (fun c -> assert_eval_val c
            (make_type
                (fullname_of_list ["foo_subtype"; "__main__"])
                (make_type (fullname_of_list ["foo_type"; "__main__"]) obj_o))
            (e [id"do";
                e [id"def"; id"x"; e [id"type"; id"foo_type"]];
                e [id"return"; e [id"type"; id"foo_subtype"; id"x"]];
                ]));

        "test_scope" >:: (fun c -> assert_eval_val c
            (make_bool true)
            (e [id"do";
                e [id"def"; id"x"; b true];
                e [id"scope"; e [id"do";
                    e [id"def"; id"x"; i 100]]];
                e [id"return"; id"x"]]
            ));

        "test_var" >:: (fun c -> assert_eval_val c
            (make_int 1)
            (e [id"do";
                e [id"def"; id"x"; e [id"var"; id"int"; i 0]];
                e [id":="; id"x"; e [id"+"; e [id"@"; id"x"]; i 1]];
                e [id"return"; e [id"@"; id"x"]]]
            ));

        "test_goto" >:: (fun c -> assert_eval_val c
            (make_int 45)
            (e [id"do";
                e [id"def"; id"x"; e [id"var"; id"int"; i 9]];
                e [id"def"; id"sum"; e [id"var"; id"int"; i 0]];
                e [id"label"; id"L1"];
                e [id":="; id"sum";
                    e [id"+";
                        e [id"@"; id"sum"];
                        e [id"@"; id"x"]]];
                e [id":="; id"x"; e [id"-"; e [id"@"; id"x"]; i 1]];
                e [id"if"; e [id">"; e [id"@"; id"x"]; i 0];
                    e [id"goto"; id"L1"]];
                e [id"return"; e [id"@"; id"sum"]]]
            ));
    ]


