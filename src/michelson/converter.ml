module StringMap = Map.Make (struct
  type t = string

  let compare = compare
end)

module IntMap = Map.Make (struct
  type t = int

  let compare = compare
end)

type 'a env =
  { var_to_stack_pos: int StringMap.t
  ; stack_pos_to_var: string IntMap.t
  ; stack_pos_to_val: 'a IntMap.t }

let stack_pos_from_var env x = StringMap.find x env.var_to_stack_pos

let add_var = StringMap.add

let remove_var = StringMap.remove

let var_from_stack_pos x env = IntMap.find x env.stack_pos_to_var

let val_from_stack_pos x env = IntMap.find x env.stack_pos_to_val

let add_stack_pos = IntMap.add

let remove_stack_pos = IntMap.remove

let next_free_var map =
  let rec aux i =
    let v = "v" ^ string_of_int i in
    if StringMap.mem v map then v else aux (i + 1)
  in
  aux 0

let push env x =
  let var_to_stack_pos =
    StringMap.map (fun pos -> pos + 1) env.var_to_stack_pos
  in
  let stack_pos_to_var =
    StringMap.fold
      (fun var pos -> add_stack_pos pos var)
      var_to_stack_pos IntMap.empty
  in
  let stack_pos_to_val =
    IntMap.fold
      (fun pos value -> add_stack_pos (pos + 1) value)
      env.stack_pos_to_val IntMap.empty
  in
  let v = next_free_var var_to_stack_pos in
  ( { var_to_stack_pos= add_var v 0 var_to_stack_pos
    ; stack_pos_to_var= add_stack_pos 0 v stack_pos_to_var
    ; stack_pos_to_val= add_stack_pos 0 x stack_pos_to_val }
  , v )

let pop env =
  let x = val_from_stack_pos 0 env in
  let stack_pos_to_val = remove_stack_pos 0 env.stack_pos_to_val in
  let v = var_from_stack_pos 0 env in
  let stack_pos_to_var = remove_stack_pos 0 env.stack_pos_to_var in
  let var_to_stack_pos = remove_var v env.var_to_stack_pos in
  ({stack_pos_to_val; stack_pos_to_var; var_to_stack_pos}, (v, x))

let drop env = fst (pop env)

let peek env =
  let x = val_from_stack_pos 0 env in
  let v = var_from_stack_pos 0 env in
  (v, x)

let dup env =
  let _, x = peek env in
  push env x

let swap env =
  let v_0 = var_from_stack_pos 0 env in
  let x_0 = val_from_stack_pos 0 env in
  let v_1 = var_from_stack_pos 1 env in
  let x_1 = val_from_stack_pos 1 env in
  let var_to_stack_pos' = add_var v_0 1 env.var_to_stack_pos in
  let var_to_stack_pos = add_var v_1 0 var_to_stack_pos' in
  let stack_pos_to_var' = add_stack_pos 0 v_1 env.stack_pos_to_var in
  let stack_pos_to_var = add_stack_pos 1 v_0 stack_pos_to_var' in
  let stack_pos_to_val' = add_stack_pos 0 x_1 env.stack_pos_to_val in
  let stack_pos_to_val = add_stack_pos 1 x_0 stack_pos_to_val' in
  {var_to_stack_pos; stack_pos_to_val; stack_pos_to_var}

let update_top_value env x =
  let stack_pos_to_val = add_stack_pos 0 x env.stack_pos_to_val in
  {env with stack_pos_to_val}

(* let rec convert_expr  *)

let rec convert env =
  let open Ast in
  let open Michelscil in
  let push_like_op env x =
    let env', v = push env x in
    let s = S_seq (S_var_decl v, S_assign (v, E_data x)) in
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
      let env', _ = dup env in
      (S_skip, env')
  | I_swap ->
      let env' = swap env in
      (S_skip, env')
  | I_push (t, x) ->
      push_like_op env x
  | I_some ->
      let v, x = peek env in
      let x' = D_some x in
      let env' = update_top_value env x' in
      (S_assign (v, E_data x'), env')
  | I_none t ->
      push_like_op env D_none
  | I_unit ->
      push_like_op env D_unit
  | I_if_none (i_t, i_f) ->
      (* let env', (v, x) = pop env in
      let e = E_binop (Eq, E_ident v, E_data D_none) in
      let s_t, _ = convert env' i_t in
      let s_f, _ = convert env' i_f in
      (S_if (e, s_t, s_f), env') *)
      (S_todo, env)
  | I_pair ->
      let env', (_, x_1) = pop env in
      let env', (_, x_2) = pop env' in
      push_like_op env' (D_pair (x_1, x_2))
  | I_car -> 
      (* let env', (_, x) = pop env in
      (match x with
      | D_pair (x', _) ->
          push_like_op env' x'
      | _ ->
          assert false) *)
          S_todo, env
      | I_cdr ->
          (* let env', (_, x) = pop env in
      (match x with
      | D_pair (_, x') ->
          push_like_op env' x'
      | _ ->
          assert false) *)
          S_todo, env
      | I_left t ->
          let env', (_, x) = pop env in
            push_like_op env' (D_left x)
      | I_right _ ->
          let env', (_, x) = pop env in
            push_like_op env' (D_right x)
      | I_if_left _ ->
          S_todo, env
      | I_if_right _ ->
          S_todo, env
      | I_nil t ->
          push_like_op env (D_list [])
      | I_cons ->
          let (v, x)
      | I_if_cons _ ->
          S_todo
      | I_size ->
          S_todo
      | I_empty_set _ ->
          S_todo
      | I_empty_map _ ->
          S_todo
      | I_map _ ->
          S_todo
      | I_iter _ ->
          S_todo
      | I_mem ->
          S_todo
      | I_get ->
          S_todo
      | I_update ->
          S_todo
      | I_if _ ->
          S_todo
      | I_loop _ ->
          S_todo
      | I_loop_left _ ->
          S_todo
      | I_lambda _ ->
          S_todo
      | I_exec ->
          S_todo
      | I_dip _ ->
          S_todo
      | I_failwith _ ->
          S_todo
      | I_cast ->
          S_todo
      | I_rename ->
          S_todo
      | I_concat ->
          S_todo
      | I_slice ->
          S_todo
      | I_pack ->
          S_todo
      | I_unpack ->
          S_todo
      | I_add ->
          S_todo
      | I_sub ->
          S_todo
      | I_mul ->
          S_todo
      | I_ediv ->
          S_todo
      | I_abs ->
          S_todo
      | I_neg ->
          S_todo
      | I_lsl ->
          S_todo
      | I_lsr ->
          S_todo
      | I_or ->
          S_todo
      | I_and ->
          S_todo
      | I_xor ->
          S_todo
      | I_not ->
          S_todo
      | I_compare ->
          S_todo
      | I_eq ->
          S_todo
      | I_neq ->
          S_todo
      | I_lt ->
          S_todo
      | I_gt ->
          S_todo
      | I_le ->
          S_todo
      | I_ge ->
          S_todo
      | I_self ->
          S_todo
      | I_contract _ ->
          S_todo
      | I_transfer_tokens ->
          S_todo
      | I_set_delegate ->
          S_todo
      | I_create_account ->
          S_todo
      | I_create_contract _ ->
          S_todo
      | I_implicit_account ->
          S_todo
      | I_now ->
          S_todo
      | I_amount ->
          S_todo
      | I_balance ->
          S_todo
      | I_check_signature ->
          S_todo
      | I_blake2b ->
          S_todo
      | I_sha256 ->
          S_todo
      | I_sha512 ->
          S_todo
      | I_hash_key ->
          S_todo
      | I_steps_to_quota ->
          S_todo
      | I_source ->
          S_todo
      | I_sender ->
          S_todo
      | I_address ->
          S_todo )
