module DA = DynArray;;

type 'a symbol =
    | Terminal of ('a -> bool)
    | NonTerm of string
;;

type 'a rule = {
    lhs: 'a symbol;
    rhs: ('a symbol) array }
;;

type 'a grammar = {
    start_symbol: 'a symbol;
    rules: ('a rule) DA.t }
;;

let n s = NonTerm s;;

let t f = Terminal f;;

let r lhs rhs = { lhs = lhs; rhs = rhs };;

let g start_symbol rules = {
    start_symbol = start_symbol;
    rules = DA.of_array rules}
;;

let str_of_symbol s :string =
    match s with
    | Terminal _ -> "t"
    | NonTerm s -> s
;;

let str_of_rule r :string =
    str_of_symbol r.lhs ^ "->" ^
        Util.joina ~sep:" " (Array.map str_of_symbol r.rhs)
;;

let str_of_grammar g :string =
    String.concat "\n" (DA.to_list (DA.map str_of_rule g.rules))


