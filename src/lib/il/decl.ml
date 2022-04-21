open! Core

module T = struct
  type t = { decl_name : string; decl_location : Label.t }
  [@@deriving ord, sexp]

  let to_string x =
    let aux (name : string) (loc : int) = [%string "%{name}^%{loc#Int}"] in
    aux x.decl_name x.decl_location
end

include T
include Comparable.Make (T)
