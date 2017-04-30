open OUnit2
open Ast
open Env
open Evaluate
open Builtin

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
let i n :ast = Atom (Imm (Int n));;
let b x :ast = Atom (Imm (Bool x));;
let s x :ast = Atom (Imm (Str_ x));;

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
                e [id"if"; id"x"; s"right"; i 0];
                ]));

        "test_return" >:: (fun c -> assert_eval_val c
            (make_bool true)
            (e [id"do";
                e [id"def"; id"x"; b true];
                e [id"return"; id"x"];
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
                e [id"type"; id"foo_subtype"; id"x"];
                ]));

        "test_scope" >:: (fun c -> assert_eval_val c
            (make_bool true)
            (e [id"do";
                e [id"def"; id"x"; b true];
                e [id"scope"; e [id"do";
                    e [id"def"; id"x"; i 100]]];
                e [id"return"; id"x"]
                ]));

    ]


