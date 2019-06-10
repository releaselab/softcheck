module StringMap = Map.Make (struct
  type t = string

  let compare = compare
end)

module IntMap = Map.Make (struct
  type t = int

  let compare = compare
end)

type 'a env =
  {var_to_stack_pos: int StringMap.t; stack_pos_to_var: string IntMap.t}

let empty_env =
  {var_to_stack_pos= StringMap.empty; stack_pos_to_var= IntMap.empty}

let stack_pos_from_var env x = StringMap.find x env.var_to_stack_pos

let add_var = StringMap.add

let remove_var = StringMap.remove

let var_from_stack_pos x env = IntMap.find x env.stack_pos_to_var

let add_stack_pos = IntMap.add

let remove_stack_pos = IntMap.remove

let next_free_var map =
  let rec aux i =
    let v = "v" ^ string_of_int i in
    if StringMap.mem v map then v else aux (i + 1)
  in
  aux 0

let push env =
  let var_to_stack_pos =
    StringMap.map (fun pos -> pos + 1) env.var_to_stack_pos
  in
  let stack_pos_to_var =
    StringMap.fold
      (fun var pos -> add_stack_pos pos var)
      var_to_stack_pos IntMap.empty
  in
  let v = next_free_var var_to_stack_pos in
  ( { var_to_stack_pos= add_var v 0 var_to_stack_pos
    ; stack_pos_to_var= add_stack_pos 0 v stack_pos_to_var }
  , v )

let pop env =
  let v = var_from_stack_pos 0 env in
  let stack_pos_to_var = remove_stack_pos 0 env.stack_pos_to_var in
  let var_to_stack_pos = remove_var v env.var_to_stack_pos in
  ({stack_pos_to_var; var_to_stack_pos}, v)

let drop env = fst (pop env)

let peek env = var_from_stack_pos 0 env

let swap env =
  let v_0 = var_from_stack_pos 0 env in
  let v_1 = var_from_stack_pos 1 env in
  let var_to_stack_pos' = add_var v_0 1 env.var_to_stack_pos in
  let var_to_stack_pos = add_var v_1 0 var_to_stack_pos' in
  let stack_pos_to_var' = add_stack_pos 0 v_1 env.stack_pos_to_var in
  let stack_pos_to_var = add_stack_pos 1 v_0 stack_pos_to_var' in
  {var_to_stack_pos; stack_pos_to_var}

let rec data_to_expr =
  let open Ast in
  let open Michelscil in
  function
  | D_int i ->
      E_int i
  | D_nat i ->
      E_nat i
  | D_string s ->
      E_string s
  | D_timestamp s ->
      E_timestamp s
  | D_signature s ->
      E_signature s
  | D_key s ->
      E_key s
  | D_key_hash s ->
      E_key_hash s
  | D_mutez s ->
      E_mutez s
  | D_contract s ->
      E_contract s
  | D_unit ->
      E_unit
  | D_bool x ->
      E_bool x
  | D_pair (x, y) ->
      E_pair (data_to_expr x, data_to_expr y)
  | D_left x ->
      E_left (data_to_expr x)
  | D_right x ->
      E_right (data_to_expr x)
  | D_some x ->
      E_some (data_to_expr x)
  | D_none ->
      E_none
  | D_list x ->
      E_list (List.map data_to_expr x)
  | D_set x ->
      E_set (List.map data_to_expr x)
  | D_map x ->
      E_map (List.map (fun (x, y) -> (data_to_expr x, data_to_expr y)) x)
  | D_instruction x ->
      let s, _ = convert empty_env x in
      E_stmt s

and convert env =
  let open Ast in
  let open Michelscil in
  let push_like_op env x =
    let env', v = push env in
    let s = S_seq (S_var_decl v, S_assign (v, x)) in
    (s, env')
  in
  function
  | I_seq (i_1, i_2) ->
      let s_1, env_1 = convert env i_1 in
      let s_2, env_2 = convert env_1 i_2 in
      (S_seq (s_1, s_2), env_2)
  | I_drop ->
      let env' = drop env in
      (S_skip, env')
  | I_dup ->
      let v_1 = peek env in
      push_like_op env (E_ident v_1)
  | I_swap ->
      let env' = swap env in
      (S_skip, env')
  | I_push (t, x) ->
      push_like_op env (data_to_expr x)
  | I_some ->
      let v = peek env in
      let x = E_some (E_ident v) in
      (S_assign (v, x), env)
  | I_none t ->
      push_like_op env E_none
  | I_unit ->
      push_like_op env E_unit
  | I_if_none (i_t, i_f) ->
      (* let env', (v, x) = pop env in
      let e = E_binop (Eq, E_ident v, E_data D_none) in
      let s_t, _ = convert env' i_t in
      let s_f, _ = convert env' i_f in
      (S_if (e, s_t, s_f), env') *)
      (S_todo, env)
  | I_pair ->
      let env', v_1 = pop env in
      let env', v_2 = pop env' in
      push_like_op env' (E_pair (E_ident v_1, E_ident v_2))
  | I_car ->
      (* let env', (_, x) = pop env in
      (match x with
      | D_pair (x', _) ->
          push_like_op env' x'
      | _ ->
          assert false) *)
      (S_todo, env)
  | I_cdr ->
      (* let env', (_, x) = pop env in
      (match x with
      | D_pair (_, x') ->
          push_like_op env' x'
      | _ ->
          assert false) *)
      (S_todo, env)
  | I_left t ->
      let env', v = pop env in
      push_like_op env' (E_left (E_ident v))
  | I_right _ ->
      let env', v = pop env in
      push_like_op env' (E_right (E_ident v))
  | I_if_left _ ->
      (S_todo, env)
  | I_if_right _ ->
      (S_todo, env)
  | I_nil t ->
      push_like_op env (E_list [])
  | I_cons ->
      let env', v_1 = pop env in
      let env', v_2 = pop env' in
      push_like_op env' (E_cons (E_ident v_1, E_ident v_2))
  | I_if_cons _ ->
      (S_todo, env)
  | I_size ->
      (S_todo, env)
  | I_empty_set _ ->
      push_like_op env (E_set [])
  | I_empty_map _ ->
      push_like_op env (E_map [])
  | I_map i ->
      let s = convert empty_env i in
      (S_todo, env)
  | I_iter _ ->
      (S_todo, env)
  | I_mem ->
      (S_todo, env)
  | I_get ->
      (S_todo, env)
  | I_update ->
      (S_todo, env)
  | I_if _ ->
      (S_todo, env)
  | I_loop _ ->
      (S_todo, env)
  | I_loop_left _ ->
      (S_todo, env)
  | I_lambda _ ->
      (S_todo, env)
  | I_exec ->
      (S_todo, env)
  | I_dip _ ->
      (S_todo, env)
  | I_failwith _ ->
      (S_todo, env)
  | I_cast ->
      (S_todo, env)
  | I_rename ->
      (S_todo, env)
  | I_concat ->
      (S_todo, env)
  | I_slice ->
      (S_todo, env)
  | I_pack ->
      (S_todo, env)
  | I_unpack ->
      (S_todo, env)
  | I_add ->
      let env', v_1 = pop env in
      let env', v_2 = pop env' in
      push_like_op env' (E_binop (Add, E_ident v_1, E_ident v_2))
  | I_sub ->
      let env', v_1 = pop env in
      let env', v_2 = pop env' in
      push_like_op env' (E_binop (Sub, E_ident v_1, E_ident v_2))
  | I_mul ->
      let env', v_1 = pop env in
      let env', v_2 = pop env' in
      push_like_op env' (E_binop (Mul, E_ident v_1, E_ident v_2))
  | I_ediv ->
      let env', v_1 = pop env in
      let env', v_2 = pop env' in
      push_like_op env' (E_binop (Div, E_ident v_1, E_ident v_2))
  | I_abs ->
      let env', v = pop env in
      push_like_op env' (E_unop (Abs, E_ident v))
  | I_neg ->
      let env', v = pop env in
      push_like_op env' (E_unop (Neg, E_ident v))
  | I_lsl ->
      let env', v_1 = pop env in
      let env', v_2 = pop env' in
      push_like_op env' (E_binop (ShiftL, E_ident v_1, E_ident v_2))
  | I_lsr ->
      let env', v_1 = pop env in
      let env', v_2 = pop env' in
      push_like_op env' (E_binop (ShiftR, E_ident v_1, E_ident v_2))
  | I_or ->
      let env', v_1 = pop env in
      let env', v_2 = pop env' in
      push_like_op env' (E_binop (Or, E_ident v_1, E_ident v_2))
  | I_and ->
      let env', v_1 = pop env in
      let env', v_2 = pop env' in
      push_like_op env' (E_binop (And, E_ident v_1, E_ident v_2))
  | I_xor ->
      let env', v_1 = pop env in
      let env', v_2 = pop env' in
      push_like_op env' (E_binop (Xor, E_ident v_1, E_ident v_2))
  | I_not ->
      let env', v = pop env in
      push_like_op env' (E_unop (Not, E_ident v))
  | I_compare ->
      (S_todo, env)
  | I_eq ->
      let env', v_1 = pop env in
      let env', v_2 = pop env' in
      push_like_op env' (E_binop (Eq, E_ident v_1, E_ident v_2))
  | I_neq ->
      let env', v_1 = pop env in
      let env', v_2 = pop env' in
      push_like_op env' (E_binop (Neq, E_ident v_1, E_ident v_2))
  | I_lt ->
      let env', v_1 = pop env in
      let env', v_2 = pop env' in
      push_like_op env' (E_binop (Lt, E_ident v_1, E_ident v_2))
  | I_gt ->
      let env', v_1 = pop env in
      let env', v_2 = pop env' in
      push_like_op env' (E_binop (Gt, E_ident v_1, E_ident v_2))
  | I_le ->
      let env', v_1 = pop env in
      let env', v_2 = pop env' in
      push_like_op env' (E_binop (Leq, E_ident v_1, E_ident v_2))
  | I_ge ->
      let env', v_1 = pop env in
      let env', v_2 = pop env' in
      push_like_op env' (E_binop (Geq, E_ident v_1, E_ident v_2))
  | I_self ->
      (S_todo, env)
  | I_contract _ ->
      (S_todo, env)
  | I_transfer_tokens ->
      (S_todo, env)
  | I_set_delegate ->
      (S_todo, env)
  | I_create_account ->
      (S_todo, env)
  | I_create_contract _ ->
      (S_todo, env)
  | I_implicit_account ->
      (S_todo, env)
  | I_now ->
      (S_todo, env)
  | I_amount ->
      (S_todo, env)
  | I_balance ->
      (S_todo, env)
  | I_check_signature ->
      (S_todo, env)
  | I_blake2b ->
      (S_todo, env)
  | I_sha256 ->
      (S_todo, env)
  | I_sha512 ->
      (S_todo, env)
  | I_hash_key ->
      (S_todo, env)
  | I_steps_to_quota ->
      (S_todo, env)
  | I_source ->
      (S_todo, env)
  | I_sender ->
      (S_todo, env)
  | I_address ->
      (S_todo, env)
