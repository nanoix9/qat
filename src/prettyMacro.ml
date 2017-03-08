open Macro;;
module S = Core.Std.String;;

type plotter = {canvas:(bytes DA.t); mutable max_x: int; mutable max_y: int}
;;

let make_plotter () =
    let can = DA.make 1 in
    DA.add can " ";
    {canvas=can; max_x=1; max_y=1}
;;

let get_width can :int = DA.length can;;
let get_height can :int = S.length (DA.get can 0);;

let expand_width can w :unit =
    let old_w = get_width can in
    if w > old_w then
        let h = get_height can in
        DA.append (DA.of_array (Array.init (w-old_w)
                (fun i -> S.make h ' '))) can
;;

let expand_height can h :unit =
    let old_h = get_height can in
    if h > old_h then
        DA.iteri (fun i e -> DA.set can i (e ^ S.make (h-old_h) ' ')) can
;;

let expand_canvas can w h :unit =
    (*Printf.printf "%d" (get_width can);*)
    expand_width can w;
    (*Printf.printf "%d" (get_width can);*)
    expand_height can h
;;

let expand_canvas_opt can w h :unit =
    let expand_if get set sz :unit =
        let old = get can in
        if sz > old then set can (max sz (old + 10))
    in
    expand_if get_width expand_width w;
    expand_if get_height expand_height h
;;

let get_line can i :string =
    DA.fold_left (fun s col -> let ch = S.get col i in s ^ S.make 1 ch) "" can
    (*S.make 1 (S.get (DA.get can 0) i)*)
;;

let get_lines can :string list =
    let lines = (S.foldi (DA.get can 0) ~init:[]
            ~f:(fun i acc _ -> get_line can i :: acc))
    in
    List.rev lines
;;

let get_content can :string =
    let lines = get_lines can in
    S.concat ~sep:"\n" lines
;;

let get_content_width can : int =
    let rec f i =
        if i >= DA.length can
                || String.length (String.trim (DA.get can i)) = 0 then
            i
        else f (i+1)
    in
    f 0
;;

let draw_point can x y label :unit =
    if x < 0 || y < 0 then raise MacroErr;
    expand_canvas_opt can (x+1) (y+1);
    S.set (DA.get can x) y label
;;

let draw_line can (x0, y0) (dx, dy) length :unit =
    let label = match dx, dy with
        | 1, 0 | -1, 0 -> '_'
        | 0, 1 | 0, -1 -> '|'
        | 1, 1 | -1, -1 -> '\\'
        | -1, 1 | 1, -1 -> '/'
        | _ -> raise MacroErr
    in
    let rec f x y cnt =
        if cnt <= 0 then ()
        else (draw_point can x y label;
            f (x+dx) (y+dy) (cnt-1))
    in
    f x0 y0 length
;;

let connect can (xfrom, yfrom) (xto, yto) :unit =
    let xdiff = xto - xfrom in
    let ydiff = yto - yfrom in
    let dx = Util.sgn xdiff in
    let dy = Util.sgn ydiff in
    let xdist = abs xdiff in
    let ydist = abs ydiff in
    if xdist > 1 then
        draw_line can (xfrom+1, yfrom) (dx, 0) (xdist-1);
    if ydist >= 1 then
        draw_line can (xto, yfrom+1) (dx, dy) 1;
    if ydist > 2 then
        draw_line can (xto, yfrom+2) (0, dy) (ydist-2)
;;

let append_sym plt label :unit =
    draw_point plt.canvas plt.max_x plt.max_y label;
    plt.max_x <- plt.max_x + 1;
    plt.max_y <- plt.max_y + 1
;;

(*let append_node plt label :unit =*)
    (*let x = plt.max_x in*)
    (*let y = plt.max_y in*)
let get_start_node g :G.V.t =
    let nodes = G.fold_vertex
        (fun v acc -> if G.pred g v = [] then v::acc else acc)
        g []
    in
    if List.length nodes <> 1 then raise MacroErr
    else List.hd nodes
;;

let str_of_dag strs_of_vlabel g :string =
    let plt = make_plotter () in
    let label_texts = DA.make 1 in
    let visited = HT.create 100 in
    let show_vert v :int * int =
        let lines = strs_of_vlabel v in
        List.iter (fun s -> DA.add label_texts s) lines;
        DA.add label_texts "";
        1, (List.length lines + 1)
    in
    let rec f v (x, y) =
        if HT.mem visited v then raise MacroErr;
        (*Printf.printf "visiting: %d\n" v;*)
        HT.add visited v true;
        draw_point plt.canvas x y '*';
        let _, h = show_vert v in
        let y_next = y + h in
        match G.succ g v with
        | [] -> (x+1, y_next)
        | lst -> List.fold_left
                (fun (xi, yi) v ->
                    connect plt.canvas (x, y) (xi, yi);
                    f v (xi, yi))
                (x, y_next) (G.succ g v)
    in
    let w, _ = f (get_start_node g) (0, 0) in
    let len = DA.length label_texts in
    expand_height plt.canvas len;
    (*let w = get_content_width plt.canvas in*)
    S.concat ~sep:"\n" (List.map2
        (fun a b -> (S.sub a 0 w) ^ " " ^  b)
        (Core.Std.List.sub (get_lines plt.canvas) 0 len)
        (DA.to_list label_texts))
;;

let str_of_dag2 str_of_vlabel g :string =
    let is_ready node g visited :bool =
        G.fold_pred (fun v acc -> acc && (HT.mem visited v)) g node true
    in
    let get_ready_nodes g visited :'a list =
        G.fold_vertex
            (fun v acc -> if is_ready v g visited then v::acc else acc)
            g []
    in
    let ready = Queue.create () in
    let visited = HT.create 100 in
    let out = DA.make 100 in
    List.iter (fun n -> Queue.add n ready) (get_ready_nodes g visited);
    while not (Queue.is_empty ready) do
        let v = Queue.take ready in
        if HT.mem visited v then raise MacroErr;
        DA.add out (str_of_vlabel (G.V.label v));
        HT.add visited v true;
        G.iter_succ
            (fun n -> if is_ready n g visited then Queue.add n ready else ())
            g v
    done;
    "grahp: " ^ string_of_int (G.nb_vertex g) ^ "\n"
        ^ (Util.join_da "\n|\n" out)
;;
