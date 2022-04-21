open! Core

type t = { decl_name : string; decl_location : Label.t }

include Comparable.S with type t := t
include Sexpable.S with type t := t

val to_string : t -> string
