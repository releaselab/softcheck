open Base
open Scil

module Make (E : Expr.S) (N : Cfg_node.S with type expr = E.t) = struct
  open N

  let free_variables free_variables_expr =
    let open N in
    let aux acc n =
      match n.stmt_s with
      | Cfg_assign (_, e) | Cfg_guard e -> Set.union acc (free_variables_expr e)
      | Cfg_call (f, args) ->
        let f_fr = free_variables_expr f in
        List.fold_left args
          ~f:(fun acc x -> Set.union acc (free_variables_expr x))
          ~init:(Set.union acc f_fr)
      | Cfg_jump | Cfg_var_decl _ -> Set.empty (module String)
    in
    aux (Set.empty (module String))

  let is_assignment ~var b =
    let open N in
    match b.stmt_s with
    | Cfg_assign (lv, _) when E.equal lv var -> true
    | Cfg_assign _ | Cfg_call _ | Cfg_guard _ | Cfg_jump | Cfg_var_decl _ ->
      false

  let aexp ~get_non_trivial_subexpr:get_non_trivial_sub_expr n =
    match n.stmt_s with
    | Cfg_assign (_, rv) -> get_non_trivial_sub_expr rv
    | Cfg_guard e -> get_non_trivial_sub_expr e
    | Cfg_call (f, args) ->
      let f' = get_non_trivial_sub_expr f in
      List.fold_left args
        ~f:(fun acc e -> Set.union acc (get_non_trivial_sub_expr e))
        ~init:f'
    | Cfg_jump | Cfg_var_decl _ -> Set.empty (module E)
end
