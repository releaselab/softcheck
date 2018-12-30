open Batteries

type ast_stmt = Ast.s_stmt

type label = Ast.label

type program = Ast.program

type func = Ast.func

type ident = Ast.ident

type expr = Ast.expr

type block =
    Sassign           of expr * expr
  | Scallassign       of ident * expr
  | Saftercallassign  of ident * expr
  | Sif               of expr
  | Soutput           of expr
  | Swhile            of expr
  | Sreturn           of expr
  | Sbegin            of ident list
  | Send

let to_string = let open Printf in function
    Sassign(lv,rv)            -> sprintf "%s = %s" (Printer.exp_to_string lv) (Printer.exp_to_string rv)
  | Scallassign (lv,rv)
  | Saftercallassign  (lv,rv) -> sprintf "%s = %s" lv (Printer.exp_to_string rv)
  | Sreturn e                 -> sprintf "return = %s" (Printer.exp_to_string e)
  | Soutput(e)                -> sprintf "output %s" (Printer.exp_to_string e)
  | Sif(e)                    -> sprintf "if %s" (Printer.exp_to_string e)
  | Swhile(e)                 -> sprintf "while %s" (Printer.exp_to_string e)
  | Sbegin _                  -> "begin"
  | Send                      -> "end"

let convert_to_simple_form p =
  let open Ast in
  let l = ref 0 in
  let label () = l := !l + 1; !l in
  let process_func f =
    let counter = ref 0 in
    let get_counter () = counter := !counter + 1; !counter in
    let rec process_expr acc e = match e with
      | Ebinop (o,e1,e2)  -> let (acc',e1') = process_expr acc e1 in
          let (acc'',e2') = process_expr acc' e2 in acc'',Ebinop (o,e1',e2')
      | Ecallf (f,vars)   -> let i = get_counter() in
          let (acc',vars') = List.fold_left (fun (a,v) var ->
            let (a',v') = process_expr a var in
            a',v' :: v) (acc,[]) vars in
          (i,(f,vars')) :: acc',Eident(Printf.sprintf "f_call_result%d" i)
      | Eunop (o,e)       -> let acc',e' = process_expr acc e in acc',Eunop(o,e')
      | Ecallfptr _
      | Ecst _
      | Eident _
      | Einput
      | Emalloc
      | Enull             -> acc,e
    in
    let process_call (i,c) =
      let lc = label() in let lr = label() in
      SScassign(lc,lr,(Printf.sprintf "f_call_result%d" i),c)
    in let rec process_stmt s =
         let s',aux_result = match s with
             Sassign (_,lv,rv) -> let acc,rv' = process_expr [] rv in
               let aux_result = List.map process_call acc in
               let s' = SSassign(label(),lv,rv') in
               s',aux_result
           | Sblock b                -> SSblock (List.map process_stmt b),[]
           | Sif (_,e,sif)           -> let acc,e' = process_expr [] e in
               let aux_result = List.map process_call acc in
               let l = label() in
               let sif' = process_stmt sif in
               let s' = SSif(l,e',sif') in
               s',aux_result
           | Sifelse (_,e,sif,selse) -> let acc,e' = process_expr [] e in
               let aux_result = List.map process_call acc in
               let l = label() in
               let sif' = process_stmt sif in
               let selse' = process_stmt selse in
               let s' = SSifelse(l,e',sif',selse') in
               s',aux_result
           | Soutput (_,e)           -> let acc,e' = process_expr [] e in
               let aux_result = List.map process_call acc in
               let s' = SSoutput (label(),e')
               in s',aux_result
           | Swhile (_,e,swhile)     -> let acc,e' = process_expr [] e in
               let aux_result = List.map process_call acc in
               let l = label() in
               let swhile' = process_stmt swhile in
               let s' = SSif(l,e',swhile') in
               s',aux_result
         in if List.length aux_result > 0 then
           SSblock (aux_result @ [s'])
         else
           s'
    in let l = label() in
    let body = process_stmt f.func_body in
    let pre_return,return = process_expr [] f.func_return in
    let pre_return' = List.map process_call pre_return in
    let s_func_body = SSblock (body :: pre_return' @ [SSreturn (label(),return)]) in
    { s_func_body = s_func_body; s_func_id = f.func_id;
      s_func_args = f.func_args; s_func_ln = l;
      s_func_vars = f.func_args; s_func_lx = label() }
  in List.map process_func p

let rec init_stmt = let open Ast in function
    SSassign(l,_,_)
  | SSoutput(l,_)
  | SSif(l,_,_)
  | SSifelse(l,_,_,_)
  | SSwhile(l,_,_)
  | SScassign (l,_,_,_)
  | SSreturn (l,_)      -> l
  | SSblock b           -> match b with [] -> assert false | _ -> List.first b |> init_stmt

let final_stmt = let open Ast in let open Set.Infix in
  let rec final_rec acc = function
      SSassign(l,_,_)
    | SSoutput(l,_)
    | SSwhile(l,_,_)
    | SScassign (_,l,_,_)
    | SSreturn (l,_)      -> acc <-- l
    | SSif (_,_,s)        -> final_rec acc s
    | SSifelse(_,_,si,se) -> final_rec (final_rec acc si) se
    | SSblock b           -> match b with [] -> assert false | _ -> List.last b |> final_rec acc
  in final_rec Set.empty

let blocks_stmt s = let open Set.Infix in
  let rec blocks_rec acc = function
      Ast.SSassign(l,lv,rv)         -> acc <-- (l,Sassign(lv,rv))
    | Ast.SSoutput(l,e)             -> acc <-- (l,Soutput(e))
    | Ast.SSreturn(l,e)             -> acc <-- (l,Sreturn(e))
    | Ast.SSblock b                 -> List.fold_left blocks_rec acc b
    | Ast.SSwhile(l,e,s)            -> blocks_rec (acc <-- (l,Swhile(e))) s
    | Ast.SSif(l,e,s)               -> blocks_rec (acc <-- (l,Sif(e))) s
    | Ast.SSifelse(l,e,si,se)       -> let acc1 = blocks_rec (acc <-- (l,Sif(e))) si in
        blocks_rec acc1 se
    | Ast.SScassign(lc,lr,lv,(f,v)) -> let rv = Ast.Ecallf (f,v) in
        acc <-- (lc,Scallassign(lv,rv)) <-- (lr,Saftercallassign(lv,rv))
  in blocks_rec Set.empty s

let labels_stmt = Set.map fst % blocks_stmt

let rev_pair x y = (y, x)

let flow_stmt s p = let open Set.Infix in let open Ast in
  let rec flow_rec acc = function
      SSassign _
    | SSoutput _
    | SSreturn _                    -> acc
    | SSblock b                     -> flow_block acc b
    | SSwhile(l,_,s)                -> flow_rec (Set.map (rev_pair l) (final_stmt s) <-- (l,init_stmt s) ||. acc) s
    | SSif(l,_,s)                   -> flow_rec (acc <-- (l,init_stmt s)) s
    | SSifelse(l,_,si,se)           -> flow_rec (flow_rec (acc <-- (l,init_stmt si) <-- (l,init_stmt se)) si) se
    | SScassign(lc,lr,_,(f,_))  -> try
          let f' = List.find (fun f' -> f'.s_func_id = f) p in
          acc <-- (lc,f'.s_func_ln) <-- (f'.s_func_lx,lr)
        with Not_found -> acc
  and flow_block acc = function
      []            -> assert false
    | [s]           -> flow_rec acc s
    | s1 :: s2 :: b -> let init_s2 = init_stmt s2 in
        let final_s1 = final_stmt s1 in
        flow_block (flow_rec (flow_rec (acc ||. Set.map (rev_pair init_s2) final_s1) s1) s2) (s2 :: b)

  in flow_rec Set.empty s

let flowR_stmt f p = flow_stmt f p |> Set.map (fun (a,b) -> (b,a))

let init_f f = f.Ast.s_func_ln

let final_f f = Set.singleton f.Ast.s_func_lx

let blocks_f f = let open Ast in let open Set.Infix in
  let blocks = blocks_stmt f.s_func_body in
  blocks <-- (f.s_func_ln,Sbegin f.s_func_args) <-- (f.s_func_lx,Send)

let labels_f = Set.map fst % blocks_f

let flow_f f p = let open Ast in let open Set.Infix in
  flow_stmt f.s_func_body p  <-- (f.s_func_ln,init_stmt f.s_func_body) ||. Set.map (rev_pair f.s_func_lx) (final_stmt f.s_func_body)

let init_p (p : Ast.s_program) = fst p |> init_f

let final_p (p : Ast.s_program) = fst p |> final_f

let blocks_p ((main,funcs) : Ast.s_program) =
  main :: funcs |> List.fold_left (fun acc f -> Set.union acc (blocks_f f)) Set.empty

let labels_p = Set.map fst % blocks_p

let flow_p p =
  List.fold_left (fun acc f -> Set.union acc (flow_f f p)) Set.empty p

let inter_flow_p p = let open Ast in let open Set.Infix in
  let rec aux acc = function
      SScassign (lc,lr,f_id,_) -> begin try
          let f = List.find (fun f' -> f'.s_func_id = f_id) p in
          acc <-- (lc,f.s_func_ln,f.s_func_lx,lr)
        with Not_found -> acc
      end
    | SSblock b                 -> List.fold_left aux acc b
    | SSif (_,_,s)
    | SSwhile (_,_,s)           -> aux acc s
    | SSifelse (_,_,s1,s2)      -> aux (aux acc s2) s1
    | SSassign _ | SSoutput _ | SSreturn _ -> acc
  in List.fold_left (fun acc f -> aux acc f.s_func_body) Set.empty p
