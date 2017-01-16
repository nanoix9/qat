open Parse

let main () =
    let argv   = Array.to_list Sys.argv in
    let args   = List.tl argv in
    let expr   = String.concat " " args in
    let result = parse expr in
        Printf.printf "%s: %s\n" expr (match result with
            | None -> ""
            | Some t -> Ast.str_of_ast t)

let () = main ()
