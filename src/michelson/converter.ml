type 'a env = 'a list

let empty_env = []

let push = List.cons

let pop = function x :: t -> (x, t) | [] -> assert false

let drop = List.tl

let peek = List.hd

let swap = function h :: h' :: t -> h' :: h :: t | _ -> assert false

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
  function
  | I_seq (i_1, i_2) ->
      let s_1, env_1 = convert env i_1 in
      let s_2, env_2 = convert env_1 i_2 in
      (S_seq (s_1, s_2), env_2)
  | I_drop ->
      (S_skip, drop env)
  | I_dup ->
      let x = peek env in
      (S_skip, push x env)
  | I_swap ->
      let env' = swap env in
      (S_skip, env')
  | I_push (t, x) ->
      (S_skip, push (data_to_expr x) env)
  | I_some ->
      let x, env' = pop env in
      (S_skip, push (E_some x) env')
  | I_none t ->
      (S_skip, push E_none env)
  | I_unit ->
      (S_skip, push E_unit env)
  | I_if_none (i_t, i_f) ->
      (* let env', (v, x) = pop env in
      let e = E_binop (Eq, x, E_data D_none) in
      let s_t, _ = convert env' i_t in
      let s_f, _ = convert env' i_f in
      (S_if (e, s_t, s_f), env') *)
      (S_todo, env)
  | I_pair ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_pair (x_1, x_2)) env')
  | I_car ->
      let x, env' = pop env in
      (S_skip, push (E_unop (Fst, x)) env')
  | I_cdr ->
      let x, env' = pop env in
      (S_skip, push (E_unop (Snd, x)) env')
  | I_left t ->
      let x, env' = pop env in
      (S_skip, push (E_left x) env')
  | I_right _ ->
      let x, env' = pop env in
      (S_skip, push (E_right x) env')
  | I_if_left _ ->
      (S_todo, env)
  | I_if_right _ ->
      (S_todo, env)
  | I_nil t ->
      (S_skip, push (E_list []) env)
  | I_cons ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_cons (x_1, x_2)) env')
  | I_if_cons _ ->
      (S_todo, env)
  | I_size ->
      (S_todo, env)
  | I_empty_set _ ->
      (S_skip, push (E_set []) env)
  | I_empty_map _ ->
      (S_skip, push (E_map []) env)
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
  | I_lambda (t_1, t_2, i) ->
      let s, _ = convert empty_env i in
      (S_skip, push (E_stmt s) env)
  | I_exec ->
      (S_todo, env)
  | I_dip i ->
      let x, env' = pop env in
      let s, env' = convert env' i in
      (s, push x env')
  | I_failwith _ ->
      (S_todo, env)
  | I_cast ->
      (S_todo, env)
  | I_rename ->
      (S_todo, env)
  | I_concat ->
      let s, env' = pop env in
      let t, env' = pop env' in
      (S_skip, push (E_concat (s, t)) env')
  | I_slice ->
      let offset, env' = pop env in
      let length, env' = pop env' in
      let x, env' = pop env' in
      (S_skip, push (E_slice (offset, length, x)) env')
  | I_pack ->
      let x, env' = pop env in
      (S_skip, push (E_pack x) env')
  | I_unpack ->
      let x, env' = pop env in
      (S_skip, push (E_unpack x) env')
  | I_add ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (Add, x_1, x_2)) env')
  | I_sub ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (Sub, x_1, x_2)) env')
  | I_mul ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (Mul, x_1, x_2)) env')
  | I_ediv ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (Div, x_1, x_2)) env')
  | I_abs ->
      let x, env' = pop env in
      (S_skip, push (E_unop (Abs, x)) env')
  | I_neg ->
      let x, env' = pop env in
      (S_skip, push (E_unop (Neg, x)) env')
  | I_lsl ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (ShiftL, x_1, x_2)) env')
  | I_lsr ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (ShiftR, x_1, x_2)) env')
  | I_or ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (Or, x_1, x_2)) env')
  | I_and ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (And, x_1, x_2)) env')
  | I_xor ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (Xor, x_1, x_2)) env')
  | I_not ->
      let x, env' = pop env in
      (S_skip, push (E_unop (Not, x)) env')
  | I_compare ->
      (S_todo, env)
  | I_eq ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (Eq, x_1, x_2)) env')
  | I_neq ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (Neq, x_1, x_2)) env')
  | I_lt ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (Lt, x_1, x_2)) env')
  | I_gt ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (Gt, x_1, x_2)) env')
  | I_le ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (Leq, x_1, x_2)) env')
  | I_ge ->
      let x_1, env' = pop env in
      let x_2, env' = pop env' in
      (S_skip, push (E_binop (Geq, x_1, x_2)) env')
  | I_self ->
      (S_skip, push E_self env)
  | I_contract _ ->
      let x, env' = pop env in
      (S_skip, push (E_contract_of_address x) env')
  | I_transfer_tokens ->
      (* let x, env' = pop env in
      let amount, env' = pop env' in
      let contract, env' = pop env' in
      (S_skip, push (E_set_delegate x) env') *)
      (S_todo, env)
  | I_set_delegate ->
      (* let x, env' = pop env in
      (S_skip, push (E_set_delegate x) env') *)
      (S_todo, env)
  | I_create_account ->
      let manager, env' = pop env in
      let delegate, env' = pop env' in
      let delegatable, env' = pop env' in
      let amount, env' = pop env' in
      ( S_skip
      , push (E_create_account (manager, delegate, delegatable, amount)) env'
      )
  | I_create_contract _ ->
      (S_todo, env)
  | I_implicit_account ->
      let x, env' = pop env in
      (S_skip, push (E_implicit_account x) env')
  | I_now ->
      (S_skip, push E_now env)
  | I_amount ->
      (S_skip, push E_amount env)
  | I_balance ->
      (S_skip, push E_balance env)
  | I_check_signature ->
      let key, env' = pop env in
      let signature, env' = pop env' in
      let bytes, env' = pop env' in
      (S_skip, push (E_check_signature (key, signature, bytes)) env')
  | I_blake2b ->
      let x, env' = pop env in
      (S_skip, push (E_blake2b x) env')
  | I_sha256 ->
      let x, env' = pop env in
      (S_skip, push (E_sha256 x) env')
  | I_sha512 ->
      let x, env' = pop env in
      (S_skip, push (E_sha512 x) env')
  | I_hash_key ->
      let x, env' = pop env in
      (S_skip, push (E_hash_key x) env')
  | I_steps_to_quota ->
      (S_skip, push E_steps_to_quota env)
  | I_source ->
      (S_skip, push E_source env)
  | I_sender ->
      (S_skip, push E_sender env)
  | I_address ->
      let x, env' = pop env in
      (S_skip, push (E_address_of_contact x) env')
