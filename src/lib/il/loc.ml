open Base

type t = Unknown | Pos of int * int [@@deriving sexp]
