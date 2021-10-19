open Base

module type S = sig
  type data

  type t = { id : int; loc : Loc.t; data : data }

  include Comparable.S with type t := t

  val to_string : t -> string

  val create : ?loc:Loc.t -> data -> t
end

module Make (T : sig
  type t [@@deriving sexp]

  val to_string : t -> string
end) =
struct
  type data = T.t [@@deriving sexp]

  module T = struct
    type t = { id : int; loc : Loc.t; data : T.t } [@@deriving sexp]

    let equal t_1 t_2 = t_1.id = t_2.id

    let compare t_1 t_2 = Int.compare t_1.id t_2.id

    let to_string t =
      Printf.sprintf "%a^%d" (fun () t -> T.to_string t.data) t t.id
  end

  include T
  include Comparable.Make (T)

  let counter = ref (-1)

  let next_counter () =
    let () = counter := !counter + 1 in
    !counter

  let create ?(loc = Loc.Unknown) data = { id = next_counter (); loc; data }
end
