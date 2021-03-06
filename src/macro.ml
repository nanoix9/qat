open Ast;;
open Txtplot;;
module S = Core.Std.String;;
module DA = DynArray;;
module HT = Hashtbl;;

exception MacroErr of string;;

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

type macro_name_elem =
    | Placeholder
    | MName of string
;;
type macro_id = macro_name_elem list;;
type macro_ast = macro_elem abs_tree;;

type macro_precedence =
    | MEquals of macro_id
    | MBetween of macro_id option * macro_id option
;;

type 'm macro = {
    id: macro_id;
    fix: fixity;
    pattern: 'm abs_tree;
    body: 'm abs_tree;
}
;;

let macro_elem_to_name e :macro_name_elem =
    match e with
    | Literal lit -> (match lit with
        | Op s | Id s -> MName s
        | _ -> raise (MacroErr
                "macro element must be identifier or operator")
        )
    | Variable _ -> Placeholder
;;

let pattern_to_id pattern :macro_id =
    let f e :macro_name_elem =
        match e with
        | Atom a -> macro_elem_to_name a
        | _ -> raise (MacroErr
                "Do not support compound astession in macro pattern")
    in
    match pattern with
    | Atom _ -> raise (MacroErr "Macro pattern must be a list")
    | NodeList el -> List.map f el
;;

let new_macro (fix :fixity)
        (patt :macro_ast)
        (body :macro_ast)
        :macro_elem macro = {
    id=pattern_to_id patt;
    fix=fix; (* TODO: checking fixity *)
    pattern=patt;
    body=body;
}
;;

let new_macro_util fix patt body :macro_elem macro =
    let to_atoms = List.map (fun x -> Atom x) in
    new_macro fix (NodeList (to_atoms patt)) (NodeList (to_atoms body))
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

let str_of_macro_name_elem n :string =
    match n with
    | Placeholder -> "_"
    | MName s -> s
;;

let str_of_macro_id i :string =
    String.concat " " (List.map str_of_macro_name_elem i)
;;

let str_of_macro_elem e :string =
    match e with
    | Literal lit -> "L(" ^ str_of_token lit ^ ")"
    | Variable v -> "V(" ^ v ^ ")"
;;

let str_of_macro_ast e :string =
    str_of_abs_tree str_of_macro_elem e
;;

let str_of_macro_precedence p :string =
    let f n =
        match n with
        | None -> "none"
        | Some e -> str_of_macro_id e
    in
    match p with
    | MEquals e -> "equals " ^ str_of_macro_id e
    | MBetween (h, l) -> "between " ^ f h ^ ", " ^ f l
;;

let str_of_macro mcr :string =
    Printf.sprintf "MACRO:\nName: %s\nFixity: %s\nPattern:\n%s\nBody:\n%s\n"
            (str_of_macro_id mcr.id)
            (str_of_fixity mcr.fix)
            (str_of_macro_ast mcr.pattern)
            (str_of_macro_ast mcr.body)
            (*(str_of_macro_precedence mcr.precedence)*)
;;

let str_of_macro_summary mcr :string =
    Printf.sprintf "%s (%s)"
            (str_of_macro_id mcr.id)
            (str_of_fixity mcr.fix)
;;


module G = struct
    (*module MacroId = struct*)
        (*type t = macro_name*)
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

module Dfs = Graph.Traverse.Dfs(G)
;;

type 'm macro_with_group = {macro:'m macro; group:int} ;;
type precedence_group = {vert:G.V.t; macros:int DA.t};;
type 'm precedences = {
    macros: ('m macro_with_group) DA.t;
    groups: precedence_group DA.t;
    dict: (macro_id, int) HT.t;
    graph: G.t}
;;

let get_start_node g :G.V.t =
    let nodes = G.fold_vertex
        (fun v acc -> if G.pred g v = [] then v::acc else acc)
        g []
    in
    if List.length nodes <> 1 then raise (MacroErr
            "Should be only 1 entry point in macro precedence hierarchy")
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
    let rec draw v (x, y) =
        if HT.mem visited v then raise (MacroErr
                "Cycle detected in macro precedence hierarchy");
        (*Printf.printf "visiting: %d, succ: %s\n" v (S.concat ~sep:"," (List.map string_of_int (G.succ g v)));*)
        HT.add visited v true;
        Txtplot.draw_point plt.canvas x y '*';
        let _, h = show_vert v in
        let y_next = y + h in
        match G.succ g v with
        | [] -> (x+1, y_next)
        | lst -> List.fold_left
                (fun (xi, yi) v ->
                    Txtplot.connect plt.canvas (x, y) (xi, yi);
                    draw v (xi, yi))
                (x, y_next) (G.succ g v)
    in
    let w, _ = draw (get_start_node g) (0, 0) in
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
        string_of_int i ^ ".\t["
            ^ string_of_int imacro ^ "] "
            ^ str_of_macro_summary (DA.get macros imacro).macro
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
    "=========== Macro names to index mapping ===========\n"
        ^ (HT.fold (fun k v acc -> acc ^ "\n" ^
                str_of_macro_id k ^ ": " ^ string_of_int v) dict "")
        ^ "\n=========== List of Precedences and Macros ===========\n"
        ^ Util.join_da "\n" (DA.mapi h groups)
        ^ "\n=========== Precedence Graph of Macros ===========\n"
        ^ str_of_dag strs_of_vert graph
;;

let get_macro prcdn i :'m macro=
    (DA.get prcdn.macros i).macro
;;

let get_macro_indices_in_pgroup
        (prcdn :'m precedences)
        (igrp :int)
        :int DA.t =
    let grp :precedence_group = DA.get prcdn.groups igrp in
    grp.macros
;;

let get_macros_in_pgroup
        (prcdn :'m precedences)
        (igrp :int)
        :('m macro) DA.t =
    let grp :precedence_group = DA.get prcdn.groups igrp in
    DA.map (fun i -> (DA.get prcdn.macros i).macro) grp.macros
;;

let get_macro_index prcdn mcr_id :int =
    HT.find prcdn.dict mcr_id
;;

let get_pgroup_index_for_macro prcdn mcr_id :int =
    let {dict;macros;groups;graph} = prcdn in
    let imcr = get_macro_index prcdn mcr_id in
    (DA.get macros imcr).group
;;

let get_higher_pgroups prcdn p :int list =
    let {dict;macros;groups;graph} = prcdn in
    let rec add_pred v =
        match G.pred graph v with
        | [] -> []
        | x -> List.concat (x :: List.map add_pred x)
    in
    add_pred (DA.get groups p).vert
;;

let get_pgroup_vert prcdn i =
    let {vert;_} = DA.get prcdn.groups i in vert
;;

let check_higher_than prcdn i j :bool =
    let vi = get_pgroup_vert prcdn i in
    let vj = get_pgroup_vert prcdn j in
    let rec has_path vi vj =
        if vi = vj then true
        else has_any_path (G.succ prcdn.graph vi) vj
    and has_any_path vs vj =
        match vs with
        | [] -> false
        | vi::vis -> has_path vi vj || has_any_path vis vj
    in
    has_path vi vj
;;

let add_precedence_group prcdn
        (high :int option)
        (low :int option)
        :int * precedence_group =
    let {dict;macros;groups;graph} = prcdn in
    (* make sure `high` is higher than `low` if both set *)
    (match high, low with
    | Some h, Some l ->
            if not (check_higher_than prcdn h l) then
            raise (MacroErr (Printf.sprintf
                "Add Precedence between group %d and %d but %d is not higher than %d"
                h l h l))
    | _ -> ());
    let i = DA.length groups in
    let v = G.V.create i in
    let grp = {vert=v; macros=DA.create ()} in
    DA.add groups grp;
    G.add_vertex graph v;
    let pred = match high with
        | None ->
                assert (DA.length groups = 1);
                None
        | Some idx -> let p = get_pgroup_vert prcdn idx in
                G.add_edge graph p v; Some p
    in
    let succ = match low with
        | None -> None
        | Some idx -> let s = get_pgroup_vert prcdn idx in
                G.add_edge graph v s; Some s
    in
    (match pred, succ with
    | Some p, Some s -> G.remove_edge graph p s
    | _ -> ());
    i, grp
;;

let add_macro_to_pgroup prcdn mcr igrp :unit =
    let {dict;macros;groups;graph} = prcdn in
    let grp = DA.get groups igrp in
    let i = DA.length macros in
    DA.add macros {macro=mcr; group=igrp};
    DA.add grp.macros i;
    HT.add dict mcr.id i;
;;

let add_macro_between_helper prcdn
        (mcr :macro_elem macro)
        (high_p :int option)
        (low_p :int option)
        :unit =
    let {dict;macros;groups;graph} = prcdn in
    if HT.mem dict mcr.id then raise (MacroErr
        ("Macro '" ^ str_of_macro_id mcr.id ^ "' already exists"))
    else begin
        let igrp, grp = add_precedence_group prcdn high_p low_p in
        add_macro_to_pgroup prcdn mcr igrp
    end
;;

let add_macro_equals prcdn
        (mcr :macro_elem macro)
        (base :macro_id)
        :unit =
    let {dict;macros;groups;graph} = prcdn in
    if HT.mem dict mcr.id then raise (MacroErr
        ("Macro '" ^ str_of_macro_id mcr.id ^ "' already exists"))
    else begin
        let igrp = get_pgroup_index_for_macro prcdn base in
        add_macro_to_pgroup prcdn mcr igrp
    end
;;

let add_macro_between prcdn mcr
        (high :(macro_id) option)
        (low :(macro_id) option)
        :unit =
    (*Printf.printf "%s\n" (str_of_precedences prcdn);*)
    let p_high = match high with
        | None -> Some 0
        | Some i -> Some (get_pgroup_index_for_macro prcdn i)
    in
    let p_low = match low with
        | None -> None
        | Some i -> Some (get_pgroup_index_for_macro prcdn i)
    in
    (*let d x = match x with None -> -1 | Some i -> i in*)
    (*Printf.printf "add between: %d %d\n" (d p_high) (d p_low);*)
    add_macro_between_helper prcdn mcr p_high p_low
;;

let add_macro (prcdn :'m precedences)
        (mcr :'m macro)
        (prec :macro_precedence)
        :unit =
    match prec with
    | MEquals eq -> add_macro_equals prcdn mcr eq
    | MBetween (h, l) -> add_macro_between prcdn mcr h l
;;

let iter_pgroup (f :G.V.label -> unit) (prcdn :'m precedences) :unit =
    let g = prcdn.graph in
    Dfs.prefix_component (fun v -> f (G.V.label v)) g (get_start_node g)
;;

let make_precedences () =
    let p = {
        dict = HT.create 100;
        macros = DA.make 100;
        groups = DA.make 10;
        graph = G.create ~size:100 () }
    in
    let m = new_macro_util Closed [Variable "_"] [] in
    add_macro_between_helper p m None None;
    p
;;

