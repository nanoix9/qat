type imm =
    | Int of int
    | Float of float
    | Str_ of string
    | Bool of bool

type token =
    | EOS
    | Imm of imm
    | Id of string
    | Op of string

type expr =
    | Atom of token
    | ExprList of expr list

let str_of_imm = function
    | Int i -> "INT(" ^ (string_of_int i) ^ ")"
    | Float f -> "FLOAT(" ^ (string_of_float f) ^ ")"
    | Str_ s -> "STR(" ^ (s) ^ ")"
    | Bool b -> "BOOL(" ^ (string_of_bool b) ^ ")"

let str_of_token = function
    | EOS -> ""
    | Imm i -> str_of_imm i
    | Id i -> i
    | Op o -> o

let rec str_of_expr = function
    | Atom t -> str_of_token t
    | ExprList e -> "(" ^ Util.join (List.map str_of_expr e) ^ ")"

