open Core.Std;;

module type OrderedHashset_sig = sig
    type 'a t
    val make: int -> 'a t
    val length: 'a t -> int
    val get: 'a t -> int -> 'a
    val add: 'a t -> 'a -> unit
    val iter: ('a -> unit) -> 'a t -> unit
    val to_list: 'a t -> 'a list
end

module OrderedHashset: OrderedHashset_sig = struct
    type 'a t = (('a DynArray.t) * ('a, unit) Hashtbl.t)

    let make size = (DynArray.make size, Hashtbl.Poly.create ())
    let length (arr, _) = DynArray.length arr
    let get (arr, _) i = DynArray.get arr i
    let add (arr, ht) elt = if not (Hashtbl.mem ht elt) then
        let _ = Hashtbl.add ht elt () in
        DynArray.add arr elt
    let iter f (arr, ht) = DynArray.iter f arr
    let to_list (arr, ht) = DynArray.to_list arr
end
