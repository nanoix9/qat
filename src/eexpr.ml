type eatom =
    | Sym of string
    | EImm of Expr.imm

type eexpr = eatom Expr.abs_expr

let str_of_eatom ea :string =
    match ea with
    | Sym s -> "SYM(" ^ s ^ ")"
    | EImm i -> Expr.str_of_imm i
;;

let str_of_eexpr = Expr.str_of_abs_expr str_of_eatom
;;


