module G = Graph.Pack.Digraph;;
open Expr;;

exception MacroErr;;

type associativity =
    | Left
    | Right
    | Non
;;

type fixity =
    | Prefix
    | Infix of associativity
    | Postfix
    | Closed
;;

type macro_elem =
    | Literal of token
    | Variable of string
;;

type macro_name =
    | Placeholder
    | Name of string
;;

type macro_expr = macro_elem abs_expr;;

type 'm macro = {
    id: macro_name list;
    fix: fixity;
    pattern: 'm abs_expr;
    body: 'm abs_expr }
;;

let macro_elem_to_name e :macro_name =
    match e with
    | Literal lit -> Name (str_of_token lit)
    | Variable _ -> Placeholder
;;

let pattern_to_name pattern :macro_name list =
    let f e :macro_name =
        match e with
        | Atom a -> macro_elem_to_name a
        | _ -> raise MacroErr
    in
    match pattern with
    | Atom _ -> raise MacroErr
    | ExprList el -> List.map f el
;;

let new_macro patt body :macro_elem macro =
    let to_atoms = List.map (fun x -> Atom x) in
    {
        id=List.map macro_elem_to_name patt;
        fix=Infix Left;
        pattern=ExprList (to_atoms patt);
        body=ExprList (to_atoms body)}
;;

let str_of_fixity f :string =
    match f with
    | Prefix -> "prefix"
    | Infix Left -> "infix left"
    | Infix Right -> "infix right"
    | Infix Non -> "infix non"
    | Postfix -> "postfix"
    | Closed -> "closed"
;;

let str_of_macro_name n :string =
    match n with
    | Placeholder -> "_"
    | Name s -> s
;;

let str_of_macro_elem e :string =
    match e with
    | Literal lit -> "L(" ^ str_of_token lit ^ ")"
    | Variable v -> "V(" ^ v ^ ")"
;;

let str_of_macro_expr e :string =
    str_of_abs_expr str_of_macro_elem e
;;

let str_of_macro mcr :string =
    Printf.sprintf "MACRO:\nName: %s\nFixity: %s\nPattern:\n%s\nBody:\n%s\n"
            (String.concat " " (List.map str_of_macro_name mcr.id))
            (str_of_fixity mcr.fix)
            (str_of_macro_expr mcr.pattern)
            (str_of_macro_expr mcr.body)
;;


(*type precedences = {*)

(*let add_macro prcdn mcr high low :unit =*)
    (*let pred = get_vert prcdn high in*)
    (*let succ = get_vert prcdn low in*)
    (*let curr = G.V.create mcr in*)
    (*add_vertex prcdn curr;*)
    (*add_edge pred curr;*)
    (*add_edge curr succ;*)
    (*remove_edge pred succ*)
(*;;*)

