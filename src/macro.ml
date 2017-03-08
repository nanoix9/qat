open Expr;;
open Txtplot;;
module S = Core.Std.String;;
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

let get_start_node g :G.V.t =
    let nodes = G.fold_vertex
        (fun v acc -> if G.pred g v = [] then v::acc else acc)
        g []
    in
    if List.length nodes <> 1 then raise MacroErr
    else List.hd nodes
;;

let str_of_dag strs_of_vlabel g :string =
    let plt = Txtplot.make_plotter () in
    let label_texts = DA.make 1 in
    let visited = HT.create 100 in
    let show_vert v :int * int =
        let lines = strs_of_vlabel v in
        List.iter (fun s -> DA.add label_texts s) lines;
        DA.add label_texts "";
        1, (List.length lines + 1)
    in
    let rec f v (x, y) =
        if HT.mem visited v then raise MacroErr;
        (*Printf.printf "visiting: %d\n" v;*)
        HT.add visited v true;
        Txtplot.draw_point plt.canvas x y '*';
        let _, h = show_vert v in
        let y_next = y + h in
        match G.succ g v with
        | [] -> (x+1, y_next)
        | lst -> List.fold_left
                (fun (xi, yi) v ->
                    Txtplot.connect plt.canvas (x, y) (xi, yi);
                    f v (xi, yi))
                (x, y_next) (G.succ g v)
    in
    let w, _ = f (get_start_node g) (0, 0) in
    let len = DA.length label_texts in
    Txtplot.expand_height plt.canvas len;
    (*let w = get_content_width plt.canvas in*)
    S.concat ~sep:"\n" (List.map2
        (fun a b -> (S.sub a 0 w) ^ " " ^  b)
        (Core.Std.List.sub (Txtplot.get_lines plt.canvas) 0 len)
        (DA.to_list label_texts))
;;

(*let str_of_dag2 str_of_vlabel g :string =*)
    (*let is_ready node g visited :bool =*)
        (*G.fold_pred (fun v acc -> acc && (HT.mem visited v)) g node true*)
    (*in*)
    (*let get_ready_nodes g visited :'a list =*)
        (*G.fold_vertex*)
            (*(fun v acc -> if is_ready v g visited then v::acc else acc)*)
            (*g []*)
    (*in*)
    (*let ready = Queue.create () in*)
    (*let visited = HT.create 100 in*)
    (*let out = DA.make 100 in*)
    (*List.iter (fun n -> Queue.add n ready) (get_ready_nodes g visited);*)
    (*while not (Queue.is_empty ready) do*)
        (*let v = Queue.take ready in*)
        (*if HT.mem visited v then raise MacroErr;*)
        (*DA.add out (str_of_vlabel (G.V.label v));*)
        (*HT.add visited v true;*)
        (*G.iter_succ*)
            (*(fun n -> if is_ready n g visited then Queue.add n ready else ())*)
            (*g v*)
    (*done;*)
    (*"grahp: " ^ string_of_int (G.nb_vertex g) ^ "\n"*)
        (*^ (Util.join_da "\n|\n" out)*)
(*;;*)

let str_of_precedences {dict;macros;groups;graph} :string =
    let f i imacro :string =
        string_of_int i ^ ".\t[" ^ string_of_int imacro ^ "] " ^ str_of_macro_id (DA.get macros imacro).macro.id
    in
    let h i (grp:precedence_group) :string =
        string_of_int i ^ ".\n\t" ^ (Util.join_da "\n\t" (DA.mapi f grp.macros))
    in
    (*Util.join_da "\n" (DA.mapi h groups)*)
    let f2 i imacro :string =
        "    " ^ str_of_macro_id (DA.get macros imacro).macro.id
    in
    let strs_of_vert v :string list =
        let igrp = (G.V.label v) in
        string_of_int igrp ::
            (List.mapi f2 (DA.to_list (DA.get groups igrp).macros))
    in
    "=========== List of Precedences and Macros ===========\n"
        ^ Util.join_da "\n" (DA.mapi h groups)
        ^ "\n=========== Precedence Graph of Macros ===========\n"
        ^ str_of_dag strs_of_vert graph
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

