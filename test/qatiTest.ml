open OUnit2
open Qatlib
open Evaluate
open Builtin
open Big_int

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

let make_int i = Builtin.make_int (big_int_of_int i);;

let suite =
    "qati" >::: [
        "test_arith" >:: (fun c -> assert_exec_val c
            (make_int 10)
            "(* 2 5)");

        "test_sym" >:: (fun c -> assert_exec_val c
            (make_int 53)
            "(def x 10; def y 3; (return (+ (* x 5) (/ x y)));)");

        "test_if" >:: (fun c -> assert_exec_val c
            (make_int 2)
            "(def x false; def y 10; if x (return (* y 5)) (return (/ y 5));)");

    ]
;;


