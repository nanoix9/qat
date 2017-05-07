open OUnit2
open Qatlib
open Evaluate
open Builtin

let assert_exec c expect code =
    let interp = make_interp () in
    let res = run interp code in
    assert_equal ~ctxt:c
        ~cmp:eq_evalret
        ~printer:str_of_evalret
        expect res
;;

let assert_exec_val c expect code =
    assert_exec c (EvalVal expect) code
;;

let suite =
    "qati" >::: [
        "test_arith" >:: (fun c -> assert_exec_val c
            (make_int 10)
            "(* 2 5)");

        "test_sym" >:: (fun c -> assert_exec_val c
            (make_int 53)
            "(def x 10; def y 3; + (* x 5) (/ x y);)");

        "test_if" >:: (fun c -> assert_exec_val c
            (make_int 2)
            "(def x false; def y 10; if x (* y 5) (/ y 5);)");

    ]
;;


