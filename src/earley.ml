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

let r lhs rhs = { lhs = n lhs; rhs = rhs };;

let g start_symbol rules = {
    start_symbol = n start_symbol;
    rules = DA.of_array rules}
;;

let join_da sep da =
    String.concat sep (DA.to_list da)

let str_of_symbol s :string =
    match s with
    | Terminal _ -> "<T>"
    | NonTerm s -> s
;;

let str_of_rule r :string =
    str_of_symbol r.lhs ^ " -> " ^
        Util.joina ~sep:" " (Array.map str_of_symbol r.rhs)
;;

let str_of_grammar g :string =
    join_da "\n" (DA.map str_of_rule g.rules)
;;

type item = {
    rule: int;
    start: int;
    next: int}
;;

let get_next_symbol gram item :('a symbol) option =
    let rhs = (DA.get gram.rules item.rule).rhs in
    if item.next >= Array.length rhs then
        None
    else
        Some (Array.get rhs item.next)
;;

let add_item s i item =
    DA.add (DA.get s i) item

let earley_match gram input =
    let predict s i non_term =
        let f k rule =
            if rule.lhs = non_term then
                add_item s i {rule=k; start=i; next=0}
        in
        DA.iteri f gram.rules
    and scan s i j check_term =
        let curr_input = input i in
        if check_term curr_input then
            let item = DA.get (DA.get s i) j in
            (if DA.length s <= i+1 then
                DA.add s (DA.make 1);
            add_item s (i+1) {item with next=item.next+1})
    and complete s i j =
        let state_set = (DA.get s i) in
        let item = DA.get state_set j in
        let matched_symbol = (DA.get gram.rules item.rule).lhs in
        let f item =
            match get_next_symbol gram item with
            | Some ((NonTerm _) as non_term) when non_term = matched_symbol ->
                    add_item s i {item with next=item.next+1}
            | _ -> ()
        in
        DA.iter f (DA.get s item.start)
    in
    let state_set_array = DA.init 1 (fun i -> DA.make 1) in
    predict state_set_array 0 gram.start_symbol;
    let i = ref 0 in
    while !i < DA.length state_set_array do
        let j = ref 0 in
        let state_set = DA.get state_set_array !i in
        while !j < DA.length state_set do
            (let item = DA.get state_set !j in
            match get_next_symbol gram item with
            | None -> complete state_set_array !i !j
            | Some ((NonTerm _) as non_term)
                -> predict state_set_array !i non_term
            | Some (Terminal f) -> scan state_set_array !i !j f
            );
            incr j
        done;
        incr i
    done;
    state_set_array
;;

let str_of_item gram item =
    let {lhs; rhs} = DA.get gram.rules item.rule in
    let already_matched = Array.sub rhs 0 item.next in
    let to_match = Array.sub rhs item.next (Array.length rhs - item.next) in
    str_of_symbol lhs
        ^ " -> "
        ^ Util.joina ~sep:" " (Array.concat [
            (Array.map str_of_symbol already_matched);
            [| "•" |];
            (Array.map str_of_symbol to_match)])
        ^ " (" ^ string_of_int item.start ^ ")"
;;

let str_of_items gram state_set_array =
    let f i state_set =
        Printf.sprintf "====== %d ======\n%s\n" i
            (join_da "\n" (DA.map (str_of_item gram) state_set))
    in
    join_da "\n" (DA.mapi f state_set_array)
;;
