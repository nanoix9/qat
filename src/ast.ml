type imm =
    | Int of string
    | Float of string
    | Str_ of string
    | Bool of string

type token =
    | EOS
    | OpenGroup of string
    | CloseGroup of string
    | Terminator of string
    | Imm of imm
    | Id of string
    | Op of string

type 'a abs_tree =
    | Atom of 'a
    | NodeList of 'a abs_tree list

type ast = token abs_tree

let literal_of_imm = function
    | Int i -> i
    | Float f -> (f)
    | Str_ s -> (s)
    | Bool b -> (b)
;;

let str_of_imm = function
    | Int i -> "INT(" ^ (i) ^ ")"
    | Float f -> "FLOAT(" ^ (f) ^ ")"
    | Str_ s -> "STR(" ^ (s) ^ ")"
    | Bool b -> "BOOL(" ^ (b) ^ ")";;

let str_of_token = function
    | EOS -> "EOS"
    | OpenGroup t -> "OPEN" ^ t
    | CloseGroup t -> "CLOSE" ^ t
    | Terminator t -> "TERMIN" ^ t
    | Imm i -> str_of_imm i
    | Id i -> "ID(" ^ i ^ ")"
    | Op o -> "OP(" ^ o ^ ")";;

let rec str_of_abs_tree str_of_a e = match e with
    | Atom t -> str_of_a t
    | NodeList e -> "[" ^ String.concat ", "
            (List.map (str_of_abs_tree str_of_a) e) ^ "]" ;;

let str_of_ast = str_of_abs_tree str_of_token ;;

let rec str_sexp_of_abs_tree str_of_a e =
    match e with
    | Atom t -> str_of_a t
    | NodeList e -> "(" ^ String.concat " "
            (List.map (str_sexp_of_abs_tree str_of_a) e) ^ ")" ;;

let pretty_sexp_of_abs_tree str_of_a e =
    let step = 2 in
    let rec helper indent e =
        match e with
        | Atom t -> false, str_of_a t
        | NodeList e ->
            let nodes = List.map (helper (indent + step)) e in
            let nodes_str = List.map (fun (m, s) -> s) nodes in
            let is_multi_lines = (List.exists (fun (m, s) -> m) nodes)
                || (List.fold_left
                    (fun len s -> len + String.length s)
                    0 nodes_str) >= 80
            in
            let str = if is_multi_lines then
                let sep = "\n" ^ String.make indent ' ' in
                    "(" ^ String.concat sep nodes_str
                     ^ ")"
                else
                    "(" ^ String.concat " " nodes_str ^ ")"
            in
            is_multi_lines, str
    in
    let _, s = helper 0 e in
    s
;;

let str_sexp_of_token = function
    | EOS -> "EOS"
    | OpenGroup t -> t
    | CloseGroup t -> t
    | Terminator t -> t
    | Imm i -> literal_of_imm i
    | Id i -> i
    | Op o -> o
;;

let str_sexp_of_ast = str_sexp_of_abs_tree str_sexp_of_token ;;
let pretty_sexp_of_ast = pretty_sexp_of_abs_tree str_sexp_of_token ;;

let rec eq_ast (e:ast) (f:ast) :bool =
    match e, f with
    | Atom t, Atom s -> t = s
    | NodeList e, NodeList f -> List.length e = List.length f &&
                                List.for_all2 eq_ast e f
    | _ -> false;;

