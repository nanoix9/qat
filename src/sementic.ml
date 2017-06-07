open Ast

exception SementicErr of string;;

let raise_err infor exp =
    raise (SementicErr (infor ^ ": " ^ str_sexp_of_ast exp))
;;

let rec validate_special_form (exp :ast) :bool =
    match exp with
    | (Atom _) as t -> true
    | NodeList nl -> validate_list nl exp
and validate_list (nl :ast list) (exp :ast) :bool =
    match nl with
    | [] -> true
    | (Atom (Id opr) as aopr)::opd -> (match opr with
        | "do" -> validate_do opd exp
        | "if" -> validate_if opd exp
        | "def" -> validate_def opd exp
        | "type" -> validate_type opd exp
        | "func" -> validate_func opd exp
        | "return" -> validate_return opd exp
        | "scope" -> validate_scope opd exp
        | "goto" -> validate_goto opd exp
        (*| "var" -> validate_var opd exp*)
        | "struct" -> validate_struct opd exp
        | _ -> validate_apply aopr opd exp
        )
    | aopr::opd -> validate_apply aopr opd exp
and validate_each (nl :ast list) :bool =
    List.for_all validate_special_form nl
and validate_do opd exp =
    match opd with
    | [] -> raise_err "DO list is empty" exp
    | _ -> validate_each opd
and validate_if opd exp =
    match opd with
    (*"if cond statement_true" or "if cond statement_true statement_false"*)
    | [_; _] | [_; _; _] -> validate_each opd
    | _ -> raise_err "IF statement invalid" exp
and validate_def opd exp =
    match opd with
    | [Atom (Id _); body] -> validate_each opd
    | _ -> raise_err "DEF statement invalid" exp
and validate_type opd exp =
    match opd with
    (* type name;
     * type name super_name; *)
    | [Atom (Id _)] | Atom (Id _)::Atom (Id _)::[] -> validate_each opd
    | _ -> raise_err "TYPE statement invalid" exp
and validate_func opd exp =
    match opd with
    | (Atom name)::(NodeList param_stmts)::body::[] ->
        (match name with
        | Id _ | Op _ -> validate_each opd
        | _ -> raise_err "FUNC statement invalid" exp)
    | _ -> raise_err "FUNC statement invalid" exp
and validate_return opd exp =
    match opd with
    | [stmt] -> validate_each opd
    | _ -> raise_err "RETURN statement invalid" exp
and validate_scope opd exp =
    match opd with
    | Atom (Id _)::[_] | [_] -> validate_each opd
    | _ -> raise_err "SCOPE statement invalid" exp
and validate_goto opd exp =
    match opd with
    | [Atom (Id label)] -> true
    | _ -> raise_err "GOTO statement invalid" exp
(*and validate_var opd =*)
and validate_struct opd exp =
    if List.length opd = 0 then
        raise_err "STRUCT statement invalid" exp
    else begin
        let f item =
            match item with
            | NodeList [Atom (Id name); value] -> validate_each opd
            | _ -> raise_err "STRUCT field invalid" item
        in
        List.for_all f opd
    end
and validate_apply aopr opd exp =
    (*match opd with*)
    (*| _ -> raise_err "" exp*)
    validate_each opd
;;

let sementic_analyze (exp :ast) :ast =
    let _ = validate_special_form exp in
    exp
;;

