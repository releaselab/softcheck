module Make (E : Set.OrderedType) = struct
  module M_set = Set.Make (E)

  type ('a, 'b) t = ('a, M_set.t) Hashtbl.t

  let is_empty h = Hashtbl.length h = 0

  let add ht k v =
    let s =
      match Hashtbl.find_opt ht k with None -> M_set.empty | Some s -> s
    in
    M_set.add v s |> Hashtbl.add ht k

  let find = Hashtbl.find

  let remove_all h = Hashtbl.iter (fun k _ -> Hashtbl.remove h k) h

  let remove = Hashtbl.remove

  let mem = Hashtbl.mem

  let iter = Hashtbl.iter

  let map f h = Hashtbl.iter (fun k d -> Hashtbl.add h k (f k d)) h

  (* let map_inplace = Hashtbl.map_inplace

     let fold = Hashtbl.fold

     let modify = Hashtbl.modify

     let modify_def = Hashtbl.modify_def

     let modify_opt = Hashtbl.modify_opt

     let enum = Hashtbl.enum

     let of_enum = Hashtbl.enum *)
end
