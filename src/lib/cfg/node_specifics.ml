open! Core
open Scil

module Make
  (E : Expr.S)
  (N : Cfg_node.S with type expr = E.t) (S : sig
    type expr = E.t

    val free_variables_expr : expr -> String.Set.t
  end) =
struct
  let free_variables =
    let aux acc n =
      match n.N.stmt_s with
      | Cfg_assign (_, e) | Cfg_guard e | Cfg_var_assign (_, e) | Cfg_return e
        ->
        Set.union acc (S.free_variables_expr e)
      | Cfg_call_var_assign (_, f, args)
      | Cfg_call_assign (_, f, args)
      | Cfg_call (f, args) ->
        List.fold_left args
          ~f:(fun acc x -> Set.union acc (S.free_variables_expr x))
          ~init:(Set.union acc (S.free_variables_expr f))
      | Cfg_var_decl _ -> String.Set.empty
    in
    aux String.Set.empty

  let is_assignment_var ~var b =
    let open N in
    match b.stmt_s with
    | Cfg_var_assign (lv, _) | Cfg_call_var_assign (lv, _, _) ->
      String.(lv = var)
    | Cfg_call_assign _ | Cfg_return _ | Cfg_assign _ | Cfg_call _ | Cfg_guard _
    | Cfg_var_decl _ ->
      false

  let is_assignment_expr ~expr b =
    let open N in
    match b.stmt_s with
    | Cfg_assign (lv, _) | Cfg_call_assign (lv, _, _) -> E.equal lv expr
    | Cfg_var_assign _ | Cfg_return _ | Cfg_call _ | Cfg_call_var_assign _
    | Cfg_guard _ | Cfg_var_decl _ ->
      false

  let aexp ~get_non_trivial_subexpr:get_non_trivial_sub_expr n =
    match n.N.stmt_s with
    | Cfg_var_assign (_, e) | Cfg_assign (_, e) | Cfg_return e | Cfg_guard e ->
      get_non_trivial_sub_expr e
    | Cfg_call_var_assign (_, _, args)
    | Cfg_call_assign (_, _, args)
    | Cfg_call (_, args) ->
      List.fold_left args
        ~f:(fun acc e -> Set.union acc (get_non_trivial_sub_expr e))
        ~init:(Set.empty (module E))
    | Cfg_var_decl _ -> Set.empty (module E)
end
