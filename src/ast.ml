type imm =
    | Int of int
    | Float of float
    | Str_ of string
    | Bool of bool

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

let str_of_imm = function
    | Int i -> "INT(" ^ (string_of_int i) ^ ")"
    | Float f -> "FLOAT(" ^ (string_of_float f) ^ ")"
    | Str_ s -> "STR(" ^ (s) ^ ")"
    | Bool b -> "BOOL(" ^ (string_of_bool b) ^ ")";;

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

let rec eq_ast (e:ast) (f:ast) :bool =
    match e, f with
    | Atom t, Atom s -> t = s
    | NodeList e, NodeList f -> List.length e = List.length f &&
                                List.for_all2 eq_ast e f
    | _ -> false;;

