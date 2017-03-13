module DA = DynArray;;
module OHS = Basic.OrderedHashset;;

exception EarleyErr of int;;

type 'a symbol =
    | Terminal of string * ('a -> bool)
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

let t n f = Terminal (n, f);;

let r lhs rhs = { lhs = n lhs; rhs = rhs };;

let g start_symbol rules = {
    start_symbol = n start_symbol;
    rules = DA.of_array rules}
;;

let str_of_symbol s :string =
    match s with
    | Terminal (n, _) -> n
    | NonTerm s -> s
;;

let str_of_rule r :string =
    str_of_symbol r.lhs ^ " -> " ^
        Util.joina " " (Array.map str_of_symbol r.rhs)
;;

let str_of_grammar g :string =
    let f i r = string_of_int i ^ ".\t" ^ str_of_rule r in
    Util.join_da "\n" (DA.mapi f g.rules)
;;

let num_rules gram :int =
    DA.length gram.rules
;;

let add_rule gram rule :unit =
    DA.add gram.rules rule
;;

type 'a parsed_symbol =
    | PTerminal of 'a
    | PNonTerm of int * int
;;

type 'a item = {
    rule: int;
    start: int;
    next: int;
    parsed_symbols: ('a parsed_symbol) array}
;;

let str_of_parsed_symbol str_of_terminal ps :string =
    match ps with
    | PTerminal t -> "\"" ^ str_of_terminal t ^ "\""
    | PNonTerm (i, j) -> Printf.sprintf "(%d,%d)" i j
;;

let str_of_item str_of_token gram i item =
    let str_of_symbol_with_parsed sym ps =
        str_of_symbol sym ^ str_of_parsed_symbol str_of_token ps in
    let {lhs; rhs} = DA.get gram.rules item.rule in
    let already_matched = Array.sub rhs 0 item.next in
    let to_match = Array.sub rhs item.next (Array.length rhs - item.next) in
    string_of_int i ^ ". "
        ^ str_of_symbol lhs
        ^ " -> "
        ^ Util.joina " " (Array.concat [
            (Array.map2 str_of_symbol_with_parsed
                already_matched item.parsed_symbols);
            [| "•" |];
            (Array.map str_of_symbol to_match)])
        ^ " (" ^ string_of_int item.start ^ ")"
;;

let str_of_items str_of_token gram state_set_array :string =
    let f i state_set =
        Printf.sprintf "====== %d ======\n%s\n" i
            (String.concat "\n"
                (List.mapi (str_of_item str_of_token gram)
                    (OHS.to_list state_set)))
    in
    Util.join_da "\n" (DA.mapi f state_set_array)
;;


let get_next_symbol gram item :('a symbol) option =
    let rhs = (DA.get gram.rules item.rule).rhs in
    if item.next >= Array.length rhs then
        None
    else
        Some (Array.get rhs item.next)
;;

let get_item s i j =
    OHS.get (DA.get s i) j
;;

let add_item s i item =
    OHS.add (DA.get s i) item
;;

let earley_match gram input =
    let predict s i non_term =
        let f k rule =
            if rule.lhs = non_term then
                add_item s i {rule=k; start=i; next=0;
                              parsed_symbols=[||]}
        in
        DA.iteri f gram.rules
    and scan s i j check_term =
        let curr_input = Array.get input i in
        if check_term curr_input then
            let item = get_item s i j in
            (if DA.length s <= i+1 then
                DA.add s (OHS.make 1);
            add_item s (i+1) {item with next=item.next+1;
                              parsed_symbols=Array.append item.parsed_symbols
                                  [| PTerminal curr_input |]})
    and complete s i j =
        let item = get_item s i j in
        let matched_symbol = (DA.get gram.rules item.rule).lhs in
        let f item =
            match get_next_symbol gram item with
            | Some ((NonTerm _) as non_term) when non_term = matched_symbol ->
                    add_item s i {item with next=item.next+1;
                                  parsed_symbols=Array.append
                                      item.parsed_symbols
                                      [| PNonTerm (i, j) |]}
            | _ -> ()
        in
        OHS.iter f (DA.get s item.start)
    in
    let state_set_array = DA.init 1 (fun i -> OHS.make 1) in
    predict state_set_array 0 gram.start_symbol;
    let i = ref 0 in
    while !i < DA.length state_set_array do
        let j = ref 0 in
        let state_set = DA.get state_set_array !i in
        while !j < OHS.length state_set do
            (let item = OHS.get state_set !j in
            match get_next_symbol gram item with
            | None -> complete state_set_array !i !j
            | Some ((NonTerm _) as non_term) ->
                    predict state_set_array !i non_term
            | Some (Terminal (n, f)) ->
                    if !i < Array.length input then
                        scan state_set_array !i !j f
            );
            incr j
        done;
        incr i
    done;
    state_set_array
;;

let get_partial_matched gram items i :int option =
    if DA.length items >= i + 1 then
        let f item =
            let rule = DA.get gram.rules item.rule in
            rule.lhs = gram.start_symbol
                && item.start = 0
                && item.next = Array.length rule.rhs
        in OHS.findf f (DA.get items i)
    else
        None
;;

let get_complete_matched gram items input =
    get_partial_matched gram items (Array.length input)
;;

let complete_matched gram items input =
    match get_complete_matched gram items input with
    | None -> false
    | Some _ -> true
;;

let recognize gram input =
    let items = earley_match gram input in
    complete_matched gram items input
;;

type 'a parse_tree =
    | Leaf of 'a
    | Tree of int * ('a parse_tree) array
;;

let rec str_of_parse_tree str_of_leaf tree =
    match tree with
    | Leaf lf -> str_of_leaf lf
    | Tree (i, t) ->
            "[" ^ (Util.joina ", "
                      (Array.map (str_of_parse_tree str_of_leaf) t))
                ^ "]("
                ^ string_of_int i
                ^ ")"
;;

let rec get_parsed_item items item :'a parse_tree=
    let pss = item.parsed_symbols in
    Tree (item.rule, Array.map (get_parsed_symbol items) pss)
and get_parsed_symbol items ps =
    match ps with
    | PTerminal t -> Leaf t
    | PNonTerm (i, j) -> get_parsed_item items (get_item items i j)
;;

let parse gram input :'a parse_tree =
    let len = Array.length input in
    let items = earley_match gram input in
    match get_complete_matched gram items input with
    | None -> raise (EarleyErr (DA.length items))
    | Some k -> get_parsed_item items (get_item items len k)
;;

