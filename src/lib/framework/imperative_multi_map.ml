module Make (S : Set.S) = struct
  type data = S.t

  type 'key t = ('key, data) Hashtbl.t

  let create n = Hashtbl.create n

  let find ht x =
    match Hashtbl.find_opt ht x with None -> S.empty | Some x -> x

  let add ht x v =
    let s = Hashtbl.find ht x in
    Hashtbl.add ht x (S.add v s)
end
