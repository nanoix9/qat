open Expr
module StrMap = Util.StrMap;;
(*let expr = Expr.expr
let token = Expr.token
*)

exception MacroErr;;

type macro_elem =
    | Literal of token
    | Variable of string
;;

type macro_expr = macro_elem Expr.abs_expr;;

type macro = { pattern: macro_expr; body: macro_expr };;

type mmap_type = expr StrMap.t;;

let macro_list = [];;

let str_of_macro_elem = function
    | Literal t -> str_of_token t
    | Variable s -> "?" ^ s
;;

let str_of_macro_expr = str_of_abs_expr str_of_macro_elem;;

let str_of_mmatch_result result :string =
    let success, mmap = result in
    let s = match mmap with
        | None -> "None"
        | Some m -> Util.str_of_strmap Expr.str_of_expr m
    in (string_of_bool success) ^ ", " ^ s
;;

let equal_mmatch_result mr1 mr2 =
    let s1, m1 = mr1 in
    let s2, m2 = mr2 in
    match s1, s2 with
    | true, false | false, true -> false
    | _ -> Util.opt_equal (StrMap.equal (=)) m1 m2
;;

let create_macro (pattern: macro_expr) (body: macro_expr) :macro =
    { pattern=pattern; body=body; } ;;

let rec mmatch (pattern: macro_expr) (expr_: expr) =
    match pattern, expr_ with
        | Atom a, e -> mmatch_atom a e
        | ExprList plist, ExprList elist -> mmatch_list plist elist
        | _, _ -> false, None

and mmatch_atom a e =
    match a, e with
        | Literal t, Atom s when t = s -> true, None
        | Variable s, e -> true, Some (StrMap.add s e StrMap.empty )
        | _ -> false, None

and mmatch_list mlist elist :bool * mmap_type option =
    let mmatch_merge success mmap mmap_tail :bool * mmap_type option =
        (if success then
            let mmap_merged = (match mmap, mmap_tail with
                | Some m1, Some m2 -> Some (StrMap.union
                                      (fun s a1 a2 -> Some a1) m1 m2)
                | None, m -> m
                | m, None -> m)
            in true, mmap_merged
        else false, None) in
    match mlist, elist with
        | [], [] -> true, Some StrMap.empty
        | m::mtail, e::etail -> (match mmatch m e with
            | false, _ -> false, None
            | true, mmap -> let success, mmap_tail = mmatch_list mtail etail in
                mmatch_merge success mmap mmap_tail)
        | _ -> false, None
;;

(*
let expand_once (m: macro) (e: expr) :expr =
    mreplace (mmatch m.pattern expr) m.body
;;
*)
