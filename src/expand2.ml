open Expr;;
open Earley;;
module DA = DynArray;;
module StrMap = Util.StrMap;;

exception MacroErr;;

type macro_elem =
    | Literal of token
    | Variable of string
;;

type macro_expr = macro_elem abs_expr;;

type 'm macro = {
    pattern: 'm abs_expr;
    body: 'm abs_expr }
;;

type ('m, 't) macro_manager = {
    macros: ('m macro) DA.t;
    gram: 't grammar }
;;

let start_symbol = "E";;

let create_macro_manager () :(macro_elem, Expr.expr) macro_manager =
    {macros=DA.make 10; gram=g start_symbol [| |]}
;;

let v s = Variable s;;
let ls s = Literal (Id s);;
let lo s = Literal (Op s);;

let new_macro patt body :macro_elem macro =
    let to_atoms = List.map (fun x -> Atom x) in
    {pattern=ExprList (to_atoms patt); body=ExprList (to_atoms body)}
;;

let str_of_macro_elem e :string =
    match e with
    | Literal t -> "L(" ^ str_of_token t ^ ")"
    | Variable v -> "V(" ^ v ^ ")"
;;

let str_of_macro_expr e :string =
    str_of_abs_expr str_of_macro_elem e
;;

let str_of_macro mcr :string =
    "MACRO:\nPATTERN:\n" ^ str_of_macro_expr mcr.pattern
            ^ "\nBODY:\n" ^ str_of_macro_expr mcr.body
            ^ "\n"
;;

let show_macro_manager mmngr :string =
    "========= Macros ========="
        ^ Util.join_da "\n" (DA.map str_of_macro mmngr.macros)
        ^ "\n========== Grammar ==========\n"
        ^ str_of_grammar mmngr.gram

;;

let add_macro mmngr macro :unit =
    DA.add mmngr.macros macro
;;

let get_macro mmngr i :'m macro =
    DA.get mmngr.macros i
;;

let is_macro mmngr i :bool =
    i < DA.length mmngr.macros
;;

let macro_to_grammar_rule i mcr :expr rule =
    let f m :(expr symbol) =
        match m with
        | Atom a ->
                (match a with
                | Literal lit -> t (fun x -> x = Atom lit)
                | Variable v -> n start_symbol)
        | ExprList _ -> raise MacroErr
    in
    match mcr.pattern with
    | Atom a -> raise MacroErr
    | ExprList el ->
            r ("M" ^ string_of_int i) (Array.of_list (List.map f el))
;;

let build_grammar mmngr :unit =
    let f i mcr :unit =
        let rule = macro_to_grammar_rule i mcr in
        add_rule mmngr.gram rule;
        add_rule mmngr.gram (r start_symbol [| rule.lhs |]);
        ()
    in
    add_rule mmngr.gram (r start_symbol [| t (fun x -> true) |]);
    DA.iteri f mmngr.macros
;;

let rec parse_tree_to_expr (tree :'a parse_tree) :expr =
    match tree with
    | Leaf lf -> lf
    | Tree (i, arr) ->
            ExprList (Array.to_list (Array.map parse_tree_to_expr arr))
;;

let parse_pattern mmngr exp :'a parse_tree =
    let arr = match exp with
        | Atom a -> raise MacroErr
        | ExprList el -> Array.of_list el
    in
    Util.println (str_of_items str_of_expr mmngr.gram (earley_match mmngr.gram arr));
    match parse mmngr.gram arr with
    | None -> raise MacroErr
    | Some t -> t
;;

let rec extract_vars_list (patt_list :macro_expr list)
                      (expr_list :expr list)
                      :expr StrMap.t =
    match patt_list, expr_list with
    | [], [] -> StrMap.empty
    | p::ps, e::es ->
            let mmap_first = extract_vars p e in
            let mmap_rest = extract_vars_list ps es in
            Util.merge_str_map mmap_first mmap_rest
    | _ -> raise MacroErr
and extract_vars_atom (patt :macro_elem) (exp :expr) :expr StrMap.t =
    match patt, exp with
    | Literal t, Atom a when t = a -> StrMap.empty
    | Variable v, _ -> StrMap.add v exp StrMap.empty
    | _ -> raise MacroErr
and extract_vars (pattern :macro_expr) (exp :expr) :expr StrMap.t =
    match pattern, exp with
    | Atom a, e -> extract_vars_atom a e
    (*TODO: change to foldr *)
    | ExprList pl,  ExprList el -> extract_vars_list pl el
    | _ -> raise MacroErr
;;

let rec substitute_vars (vars :expr StrMap.t) (body :macro_expr) :expr =
    match body with
    | Atom a ->
            (match a with
            | Literal t -> Atom t
            | Variable v -> StrMap.find v vars)
    | ExprList mel -> ExprList (List.map (substitute_vars vars) mel)
;;

let expand_macro (mcr :'m macro) (exp :expr) :expr =
    let vars = extract_vars mcr.pattern exp in
    substitute_vars vars mcr.body
;;

let rec expand_non_macro mmngr i arr :expr =
    if Array.length arr <> 1 then
        raise MacroErr
    else
        expand_parse_tree mmngr (Array.get arr 0)
and expand_rule mmngr i arr :expr =
    if is_macro mmngr i then
        let f = expand_parse_tree mmngr in
        expand_macro (get_macro mmngr i)
                (* expand inner first, i.e. depth-first *)
                (ExprList (Array.to_list (Array.map f arr)))
    else
        expand_non_macro mmngr i arr
and expand_parse_tree mmngr ptree :expr =
    match ptree with
    | Leaf lf -> lf
    | Tree (i, t) -> expand_rule mmngr i t
;;

let expand mmngr exp =
    let tree = parse_pattern mmngr exp in
    expand_parse_tree mmngr tree
;;
