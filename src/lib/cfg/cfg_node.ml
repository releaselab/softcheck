open Utils
open Scil

module Make (S : Softlang.S) = struct
  include S

  module Stmt = struct
    module T = struct
      type stmt =
        | Cfg_var_decl of decl
        | Cfg_assign of expr * expr
        | Cfg_guard of expr
        | Cfg_jump
        | Cfg_call of expr * expr list

      and t = { stmt_label : label; stmt_s : stmt }

      let to_string s =
        let aux = function
          | Cfg_var_decl v -> [%string "var %{v#Decl}"]
          | Cfg_assign (lv, rv) -> [%string "%{lv#Expr} := %{rv#Expr}"]
          | Cfg_guard e -> [%string "test %{e#Expr}"]
          | Cfg_jump -> "jump"
          | Cfg_call (f, args) ->
            [%string
              "%{f#Expr} %{List.to_string ~fst:\"(\" ~lst:\")\" ~sep:\", \" \
               Expr.to_string args}"]
        in
        [%string "%{aux s.stmt_s} ^ %{s.stmt_label#Int}"]

      let compare s_1 s_2 = Int.compare s_1.stmt_label s_2.stmt_label

      let equal s_1 s_2 = s_1.stmt_label = s_2.stmt_label
    end

    include T
    include Utils.Collections.Make (T)
  end

  type stmt = Stmt.t
end

module type S = sig
  include Scil.Softlang.S

  module Stmt : sig
    type stmt =
      | Cfg_var_decl of decl
      | Cfg_assign of expr * expr
      | Cfg_guard of expr
      | Cfg_jump
      | Cfg_call of expr * expr list

    and t = { stmt_label : label; stmt_s : stmt }

    include Utils.Collections.WithCollections with type t := t
  end

  type stmt = Stmt.t
end
