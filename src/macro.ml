open Expr;;
module DA = DynArray;;
module HT = Hashtbl;;

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

let new_macro fix patt body :macro_elem macro =
    let to_atoms = List.map (fun x -> Atom x) in {
        id=List.map macro_elem_to_name patt;
        fix=fix; (* TODO: checking fixity *)
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

let str_of_macro_id i :string =
    String.concat " " (List.map str_of_macro_name i)
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
            (str_of_macro_id mcr.id)
            (str_of_fixity mcr.fix)
            (str_of_macro_expr mcr.pattern)
            (str_of_macro_expr mcr.body)
;;


module G = struct
    (*module MacroId = struct*)
        (*type t = macro_name list*)
        (*let compare = Pervasives.compare*)
        (*let equal = (=)*)
        (*let hash = HT.hash*)
    (*end*)
    module Int = struct
        type t = int
        let compare = Pervasives.compare
        let hash = HT.hash
        let equal = (=)
        let default = -1
    end
    include Graph.Imperative.Digraph.Concrete(Int)
end
;;

type 'm macro_with_group = {macro:'m macro; group:int} ;;
type precedence_group = {vert:G.V.t; macros:int DA.t};;
type 'm precedences = {
    macros: ('m macro_with_group) DA.t;
    groups: precedence_group DA.t;
    dict: (macro_name list, int) HT.t;
    graph: G.t}
;;

let str_of_precedences {dict;macros;groups;graph} :string =
    let f i imacro :string =
        string_of_int i ^ ".\t[" ^ string_of_int imacro ^ "] " ^ str_of_macro_id (DA.get macros imacro).macro.id
    in
    let h i (grp:precedence_group) :string =
        string_of_int i ^ ".\n\t" ^ (Util.join_da "\n\t" (DA.mapi f grp.macros))
    in
    Util.join_da "\n" (DA.mapi h groups)
;;

let add_precedence_group {dict;macros;groups;graph}
        (high :int option)
        (low :int option)
        :int * precedence_group =
    let get_vert i =
        let {vert;_} = DA.get groups i in vert
    in
    let i = DA.length groups in
    let v = G.V.create i in
    let grp = {vert=v; macros=DA.create ()} in
    DA.add groups grp;
    G.add_vertex graph v;
    let pred = match high with
        | None -> if DA.length groups <> 1 then raise MacroErr; None
        | Some idx -> let p = get_vert idx in
                G.add_edge graph p v; Some p
    in
    let succ = match low with
        | None -> None
        | Some idx -> let s = get_vert idx in
                G.add_edge graph v s; Some s
    in
    (match pred, succ with
    | Some p, Some s -> G.remove_edge graph p s
    | _ -> ());
    i, grp
;;


let add_macro_between prcdn
        (mcr :macro_elem macro)
        (high :int option)
        (low :int option)
        :unit =
    let {dict;macros;groups;graph} = prcdn in
    if HT.mem dict mcr.id then raise MacroErr
    else begin
        let igrp, grp = add_precedence_group prcdn high low in
        let i = DA.length macros in
        DA.add macros {macro=mcr; group=igrp};
        DA.add grp.macros i;
        HT.add dict mcr.id i;
    end
;;

let add_macro prcdn mcr
        (high :(macro_name list) option)
        (low :(macro_name list) option)
        :unit =
    let high_idx = match high with
        | None -> Some 0
        | Some i -> Some (HT.find prcdn.dict i)
    in
    let low_idx = match low with
        | None -> None
        | Some i -> Some (HT.find prcdn.dict i)
    in
    add_macro_between prcdn mcr high_idx low_idx
;;

let make_precedences () = let p = {
        dict = HT.create 100;
        macros = DA.make 100;
        groups = DA.make 10;
        graph = G.create ~size:100 () }
    in
    add_macro_between p (new_macro Closed [Variable "_"] []) None None;
    p
;;

