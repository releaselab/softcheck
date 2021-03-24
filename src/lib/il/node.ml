open Utils

module type S = sig
  type data

  type t = { id : int; loc : Loc.t; data : data }

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t

  val equal : t -> t -> bool

  val compare : t -> t -> int

  val to_string : t -> string

  val create : ?loc:Loc.t -> data -> t
end

module Make_collections (E : Sig.Ordered) = struct
  module Set = Set.Make (E)
  module Map = Map.Make (E)
end

module Make (T : sig
  type t

  val to_string : t -> string
end) =
struct
  type data = T.t

  module T = struct
    type t = { id : int; loc : Loc.t; data : T.t }

    let equal t_1 t_2 = t_1.id = t_2.id

    let compare t_1 t_2 = Int.compare t_1.id t_2.id

    let to_string t =
      Printf.sprintf "%a^%d" (fun () t -> T.to_string t.data) t t.id
  end

  include T
  include Make_collections (T)

  let counter = ref (-1)

  let next_counter () =
    let () = counter := !counter + 1 in
    !counter

  let create ?(loc = Loc.Unknown) data = { id = next_counter (); loc; data }
end
