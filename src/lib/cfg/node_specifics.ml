open Scil

module Make (N : Cfg_node.S) = struct
  open N

  let free_variables free_variables blocks =
    let open N in
    let aux n acc =
      match n.Stmt.stmt_s with
      | Stmt.Cfg_assign (_, e) | Cfg_guard e ->
        Decl.Set.union acc (free_variables e)
      | Cfg_call (f, args) ->
        let f_fr = free_variables f in
        List.fold_left
          (fun acc x -> Decl.Set.union acc (free_variables x))
          (Decl.Set.union acc f_fr) args
      | Cfg_jump | Cfg_var_decl _ -> Decl.Set.empty
    in
    Stmt.Set.fold aux blocks Decl.Set.empty

  let find_assignments blocks x =
    let open N in
    let aux b acc =
      match b.Stmt.stmt_s with
      | Cfg_assign (lv, _) when Expr.equal lv x -> Stmt.Set.add b acc
      | Cfg_assign _ | Cfg_call _ | Cfg_guard _ | Cfg_jump | Cfg_var_decl _ ->
        acc
    in
    Stmt.Set.fold aux blocks Stmt.Set.empty

  let aexp get_non_trivial_subexpr n =
    match n.Stmt.stmt_s with
    | Cfg_assign (_, rv) -> get_non_trivial_subexpr rv
    | Cfg_guard e -> get_non_trivial_subexpr e
    | Cfg_call (f, args) ->
      let f' = get_non_trivial_subexpr f in
      List.fold_left
        (fun acc e -> Expr.Set.union acc (get_non_trivial_subexpr e))
        f' args
    | Cfg_jump | Cfg_var_decl _ -> Expr.Set.empty
end
