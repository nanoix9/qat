module DA = DynArray;;
module OHS = Basic.OrderedHashset;;

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

let r lhs rhs = { lhs = n lhs; rhs = rhs };;

let g start_symbol rules = {
    start_symbol = n start_symbol;
    rules = DA.of_array rules}
;;

let join_da sep da =
    String.concat sep (DA.to_list da)

let str_of_symbol s :string =
    match s with
    | Terminal _ -> "T"
    | NonTerm s -> s
;;

let str_of_rule r :string =
    str_of_symbol r.lhs ^ " -> " ^
        Util.joina ~sep:" " (Array.map str_of_symbol r.rhs)
;;

let str_of_grammar g :string =
    join_da "\n" (DA.map str_of_rule g.rules)
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

let get_next_symbol gram item :('a symbol) option =
    let rhs = (DA.get gram.rules item.rule).rhs in
    if item.next >= Array.length rhs then
        None
    else
        Some (Array.get rhs item.next)
;;

let add_item s i item =
    OHS.add (DA.get s i) item

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
            let item = OHS.get (DA.get s i) j in
            (if DA.length s <= i+1 then
                DA.add s (OHS.make 1);
            add_item s (i+1) {item with next=item.next+1;
                              parsed_symbols=Array.append item.parsed_symbols
                                  [| PTerminal curr_input |]})
    and complete s i j =
        let state_set = (DA.get s i) in
        let item = OHS.get state_set j in
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
    while !i <= Array.length input do
        let j = ref 0 in
        let state_set = DA.get state_set_array !i in
        while !j < OHS.length state_set do
            (let item = OHS.get state_set !j in
            match get_next_symbol gram item with
            | None -> complete state_set_array !i !j
            | Some ((NonTerm _) as non_term) ->
                    predict state_set_array !i non_term
            | Some (Terminal f) ->
                    if !i < Array.length input then
                        scan state_set_array !i !j f
            );
            incr j
        done;
        incr i
    done;
    state_set_array
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
        ^ Util.joina ~sep:" " (Array.concat [
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
    join_da "\n" (DA.mapi f state_set_array)
;;

let recognize gram input =
    let len = Array.length input in
    let items = earley_match gram input in
    if DA.length items >= len + 1 then
        let rec find state_set i =
            if i >= OHS.length state_set then
                false
            else (let item = OHS.get state_set i in
                let rule = DA.get gram.rules item.rule in
                if rule.lhs = gram.start_symbol
                    && item.start = 0
                    && item.next = Array.length rule.rhs then
                    true
                else
                    find state_set (i+1))
        in find (DA.get items len) 0
    else
        false
;;


