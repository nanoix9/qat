open Ast
open Env
module DA = DynArray;;

type module_t = {
    name: string list;
    path: string;
    ast: ast;
    (*expanded: ast option;*)
    (*pre_evaluated: estmt option;*)
}
;;

type module_manager = {
    pathes: DA.t;
    loaded_modules: (string list, module_t) Hashtbl.t}
;;

let load_module (mng :module_manager) (name :string) :unit =
    let file = find_source_file mng name in
    let text = Util.read_file name in
    let code = Parse.parse text in
    ()
;;
