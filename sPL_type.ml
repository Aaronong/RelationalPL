#include "xdebug.cppo"

open SPL
open Debug
(* open Gen *)

module C = SPLc

type env_type = sPL_type Environ.et
let tail_optimize_flag = ref false
let pa_removal_flag = ref true
let stack_size = ref 10000

let option_flag = [
  ("--tail", Arg.Set tail_optimize_flag, "Enable tail-call optimization.")
;("--dis-pa", Arg.Clear pa_removal_flag, "Disable partial application Removal.")
;("-stk-size", Arg.Set_int stack_size,
  "Size of Stack Memory (default is 10000)")
(* ;("-dre", Arg.String (fun s -> *)
(*     DebugCore.z_debug_file:=("$"^s); DebugCore.z_debug_flag:=true), *)
(*  "Shorthand for -debug-regexp") *)
]@command_args

let _ = Debug.parse option_flag 

(* if (v,r) in env, return Some r *)
(* otherwise, return None *)
let get_type env v = Environ.get_val env v

(* match a function type t with its parameters args *)
(* and return its residual *)
(* extr_arg_type (t1->t2->t3->t4) [e1,e2] 
   ==> Some ([(e1,t1);(e2,t2)], t3->t4) *)
(* match a function type t with its parameters args *)
(* and return its residual *)
(* extr_arg_type (t1->t2->t3->t4) [e1,e2] ==> Some ([(e1,t1);(e2,t2)], t3->t4) *)
(* use test harness below and run ./splt *)
let extr_arg_type (t:sPL_type) (args:'a list) : (('a * sPL_type) list * sPL_type) option =
  let rec aux env t args =
    match args,t with
      | [],_ -> Some (env,t)
      | v::vs,Arrow (t1,t2) -> aux (env@[(v,t1)]) t2 vs
      | _,_ -> None
  in aux [] t args

let extr_arg_type_test (t:sPL_type) (args:int list) : ((int * sPL_type) list * sPL_type) option =
  let pr1 = string_of_sPL_type in
  let pr2 = pr_list string_of_int in
  let pr2a = pr_list (pr_pair string_of_int pr1) in
  let pr3 = pr_option (pr_pair pr2a pr1) in
    Debug.no_2 "extr_arg_type_test" pr1 pr2 pr3 extr_arg_type t args

(* test harness below to debug extr_arg_type *)
(* please comment them after fixing bug *)
(* let () = y_binfo_pp "Testing extr_arg_type_test\n";; *)
let t1 = Arrow (IntType,Arrow (BoolType,IntType))
let _ = x_add extr_arg_type_test t1 [1]
let _ = x_add extr_arg_type_test t1 [1;2]
let _ = x_add extr_arg_type_test t1 [1;2;3]

(* CHANGED *)
let placeholderify (t:sPL_type) : sPL_type =
  match t with
    | RelationType (lst) -> 
        begin
          let mod_lst = List.map (fun (tag, t1) -> ("placeholder", t1)) lst in
            RelationType (mod_lst)
        end
    | _ -> t

(* type checking method *)
(* you may use this method to check that the your inferred *)
(* type is correct *)
let type_check (env:env_type) (e:sPL_expr) (t:sPL_type) : bool =
  let rec aux env e t =
    match e with
      | IntConst _ -> 
          if t=IntType then true else false
      | BoolConst _ -> 
          if t=BoolType then true else false
      | Var v ->
          (match get_type env v with
            | Some t2 -> t=t2
            | None -> false (* failwith ("type-check : var "^v^" cannot be found") *)
          )
      | UnaryPrimApp (op,arg) ->
          begin
            match op,t with
              | "~",IntType 
                -> aux env arg IntType
              | "\\",BoolType 
                -> aux env arg BoolType
              | _,_ 
                -> false
          end
      | BinaryPrimApp (op,arg1,arg2) ->
          begin
            match op,t with
              | "+",IntType | "-",IntType | "*",IntType | "/",IntType 
                -> (aux env arg1 IntType) && (aux env arg2 IntType)
              | "<",BoolType | ">",BoolType | "=",BoolType 
                -> (aux env arg1 IntType) && (aux env arg2 IntType)
              | "|",BoolType | "&",BoolType 
                -> (aux env arg1 BoolType) && (aux env arg2 BoolType)

              (* CHANGED *)
              | "|><|",RelationType(_) 
                -> 
                  begin
                    match arg1,arg2 with
                      | Relation(t1,e1), Relation(t2,e2)
                        -> (aux env arg1 t1) && (aux env arg2 t2)
                      | _,_ -> false
                  end
              | "|||",RelationType(_)
                ->
                  begin
                    match arg1,arg2 with
                      | Relation(RelationType(l1),e1), Proj projections
                        -> (aux env arg1 (RelationType(l1))) && List.for_all (fun x -> List.exists (fun (id,t) -> id = x) l1) projections
                      | _,_ -> false
                  end

              | _,_ -> false
          end
      | Cond (e1,e2,e3) ->
          let b1 = aux env e1 BoolType in
          let b2 = aux env e2 t in
          let b3 = aux env e3 t in
            b1 && b2 && b3
      | Func (te,args,body) ->
          if te=t then
            match extr_arg_type te args with
              | Some (env2,t2) -> aux (env2@env) body t2
              | None -> false (* mismatch in number of arguments *)
          else false
      | RecFunc (te,id,args,body) ->
          if te=t then
            match extr_arg_type te args with
              | Some (env2,t2) -> aux ((id,te)::env2@env) body t2
              | None -> false (* mismatch in number of arguments *)
          else false
      | Appln (e1,t1,args) ->
          begin
            match t1 with
              | Some t1a ->
                  begin
                    match extr_arg_type t1a args with
                      | Some (l2,t2) ->
                          if t=t2 then List.for_all (fun (ea,ta) -> aux env ea ta) l2
                          else false
                      | None -> false
                  end
              | None -> failwith "missing type : should call type_infer first"
          end
      | Let (ldecl,te,body) ->
          if te=t then
            let env2 = List.map (fun (t,v,b) -> (v,t)) ldecl in
            let nenv = env2@env in
              (aux nenv body te) && List.for_all (fun (t,_,b) -> aux nenv b t) ldecl
          else false

      (* CHANGED *)
      | Row row_content -> 
          begin
            match t with
              | RelationType (type_tuples) ->
                  begin
                    let row_type = List.map2 (fun (id,t) e -> (e,t)) type_tuples row_content in
                    List.for_all (fun (ea,ta) -> aux env ea ta) row_type
                  end
              | _ -> false
          end

      | Relation (t1, rows) ->
          if t1 = t then
            let row_verify = List.rev_map (fun row -> aux env row t) rows in
            List.fold_left (fun a b -> a && b) true row_verify
          else false
      | Proj _ -> if t=ProjType then true else false

  in aux env e t


let rec bulk_infer (env:env_type) (ext: ('a * sPL_type) list) (args: sPL_expr list): (sPL_expr list) =
  match ext, args with
    |(var, typ)::b,h::t -> let (at1,na1) = type_infer_x env h in
          if type_check env na1 typ then (na1::bulk_infer env b t) else []
    |[],[] -> []
    |_,_ -> []

and

  bulk_wrap (env:env_type) (ext: ('a * sPL_type) list) (args: sPL_expr list): (sPL_expr list) = 
  let p_args = bulk_infer env ext args in
    if (List.length p_args) = (List.length ext) && (List.length p_args) = (List.length args) then p_args else []

and

  (* type inference, note that None is returned  *)
  (*    if no suitable type is inferred *)
  type_infer_x (env:env_type) (e:sPL_expr) : sPL_type option * sPL_expr =
  match e with
    | IntConst _ -> (Some IntType,e)
    | BoolConst _ -> (Some BoolType,e)
    | Var v -> (get_type env v,e)
    | UnaryPrimApp (op,arg) ->
        begin
          match op with
            | "~" ->
                let (at2,na2) = type_infer_x env arg in
                  (match at2 with
                    | Some IntType -> (at2, UnaryPrimApp (op,na2))
                    | _ -> (None,e))
            | "\\" ->
                let (at2,na2) = type_infer_x env arg in
                  (match at2 with
                    | Some BoolType -> (at2, UnaryPrimApp (op,na2))
                    | _ -> (None,e))
            | _ -> (None,e)
        end
    | BinaryPrimApp (op,arg1,arg2) ->
        begin
          match op with
            | "-" | "+" | "*" | "/"  ->
                let (at1,na1) = type_infer_x env arg1 in
                let (at2,na2) = type_infer_x env arg2 in
                  (match at1,at2 with
                    | Some IntType,Some IntType -> (at2, BinaryPrimApp (op,na1,na2))
                    | _ -> (None,e))
            | "<" | ">" | "=" ->
                let (at1,na1) = type_infer_x env arg1 in
                let (at2,na2) = type_infer_x env arg2 in
                  (match at1,at2 with
                    | Some IntType,Some IntType -> (Some BoolType, BinaryPrimApp (op,na1,na2))
                    | _ -> (None,e))
            | "&" | "|" ->
                let (at1,na1) = type_infer_x env arg1 in
                let (at2,na2) = type_infer_x env arg2 in
                  (match at1,at2 with
                    | Some BoolType,Some BoolType -> (Some BoolType, BinaryPrimApp (op,na1,na2))
                    | _ -> (None,e))
            (* CHANGED *)
            | "|><|" ->
                let (at1,na1) = type_infer_x env arg1 in
                let (at2,na2) = type_infer_x env arg2 in
                  (match at1,at2 with
                    | Some RelationType(l1),Some RelationType(l2) ->
                      begin
                        let l3 = (List.merge (fun a b -> 0) l1 l2) in
                        let cons_uniq xs x = if List.mem x xs then xs else x :: xs in
                        let remove_from_right xs = List.rev (List.fold_left cons_uniq [] xs) in
                        let l4 = (remove_from_right l3) in
                        (*(Some RelationType(l3), BinaryPrimApp (op,na1,na2)) *)
                        (Some (RelationType(l4)),BinaryPrimApp (op,na1,na2))
                      end
                    | _ -> (None,e))
            | "|||" ->
                let (at1,na1) = type_infer_x env arg1 in
                let (at2,na2) = type_infer_x env arg2 in
                  (match at1,at2, na2 with
                    | Some RelationType(l1),Some ProjType, Proj(l3) -> 
                      begin
                        let good_proj_headers = List.filter (fun x -> (List.exists ( fun (id,typ) -> x=id) l1)) l3 in
                        let l2  = (List.filter (fun (id,typ) -> (List.exists (fun x -> x=id) l3) ) l1) in
                          if good_proj_headers = l3 then (Some (RelationType(l2)),BinaryPrimApp (op,na1,na2)) else (None,e) 
                      end
                    | _ -> (None,e))
            | _ -> (None,e)

        end
    | Cond (e1,e2,e3) ->
        begin
          (* e1 must be bool type *)
          (* e2,e3 must be of the same inferred type *)
          let (at1,na1) = type_infer_x env e1 in
          let (at2,na2) = type_infer_x env e2 in
          let (at3,na3) = type_infer_x env e3 in
            match at1 with
              |Some BoolType -> if at2 = at3 then (at2, Cond (na1, na2, na3))
                  else (None, e)
              |_ -> (None,e)
        end
    | Func (te,args,body) ->
        (* te is the inferred function type *)
        (* infer the types of args and body *)
        (* args and body type must be consistent with te *)
        (* extend the env when checking type of body *)
        begin
          let ret = extr_arg_type te args in
            match ret with
              |Some (ext,func_type) -> 
                  let (at2,na2) = type_infer_x (ext@env) body in
                    if at2 = (None) then (None,e) else (Some te,Func(te,args,na2))
              |None -> (None,e)

        end


    | RecFunc (te,id,args,body) -> 
        (* te is the inferred function type *)
        (* infer the types of args and body *)
        (* args and body type must be consistent with te*)
        (* extend the env when checking type of body *)
        begin
          let ret = extr_arg_type te args in
            match ret with
              |Some (ext,func_type) -> 
                  let (at2,na2) = type_infer_x ((id,te)::ext@env) body in
                    if at2 = (None) then  (None,e) else (Some te,RecFunc(te,id,args,na2))
              |None -> (None,e)

        end

    | Appln (e1,_,args) ->
        (* infer the type of e1 first *)
        (* infer the types of args *)
        (* check that args are consistent with inferred type *)
        (* remember to update _ with inferred type of e1 *)
        begin
          let (at1,na1) = type_infer_x env e1 in
            match at1 with
              |Some (t1) ->
                  begin
                    let ret = extr_arg_type t1 args in
                      match ret with
                        |Some (ext, func_type) ->
                            let p_args = bulk_wrap env ext args in
                              ((Some func_type),Appln(na1, at1, p_args))
                        |None -> (None, e)
                  end
              |None -> (None, e)
        end

    | Let (ldecl,te,body) -> 
        (* the implementation for Let is given *)
        (* pick the type of local vars from ldecl *)
        let env2 = List.map (fun (t,v,b) -> (v,t)) ldecl in
        (* build an extended type environment for checking body *)
        let nenv = env2@env in
        (* infer the type of body *)
        let (nt1,nbody) = type_infer_x nenv body in
        (* infer the type of local definitions *)
        let ls_res = List.map (fun (t,v,b) -> (type_infer_x env b,v,t)) ldecl in
          (* why did we use env rather than nenv when checking ldecl? *)
          begin
            match nt1 with
              | Some t1 -> 
                  (* check that body type is consistent *)
                  if t1=te then 
                    (* check that local declarations are typed consistently *)
                    if List.for_all (fun ((t,e),_,t2) -> t=Some t2) ls_res then
                      (nt1, Let(List.map (fun ((_,e),v,t)->(t,v,e)) ls_res,te,nbody))
                    else (None,e)
                  else (None,e)
              | None -> (None,e)
          end

    (* CHANGED *)
    | Row (args) ->
        let processed_row = List.map (fun arg -> type_infer_x env arg) args in
        let new_expressions = List.map (fun (t, e) -> e) processed_row in
        let inferred_types = List.map (fun (t, e) -> ("placeholder", t)) processed_row in
        if List.exists (fun (_, t) -> t = None) inferred_types then
          (None, e) else
          (* Casting to inttype will never occur due to the existance check above *)
          let inferred_types2 = List.map(fun (id, t) -> match t with Some t1 -> (id, t1) |None -> (id, IntType)) inferred_types in
          (Some (RelationType (inferred_types2)), Row(new_expressions))

    | Relation (t1, args) ->
        let inferred_rows = List.map (fun row -> type_infer_x env row) args in
        let new_expressions = List.map (fun (t, e) -> e) inferred_rows in
        let inferred_types = List.map (fun (t, e) -> t) inferred_rows in
        (* We wanna return the expr with the new_expressions of each row For type we 
        check all the rows have same type then check it against the maintype t1, if true return some of t1 else return none*)
        let mod_t1 = placeholderify t1 in 
        if List.exists (fun t -> t = None) inferred_types then
          (None, e) else
          let inferred_types2 = List.map(fun t -> match t with Some t1 -> t1 |None -> (IntType)) inferred_types in
            if List.fold_left (fun a b -> a && b) true (List.map (fun t -> t = mod_t1) inferred_types2) then
              (Some(t1), Relation(t1, new_expressions)) else
              (None, e)

    | Proj _ -> (Some ProjType,e)

;;

let rec type_infer (env:env_type) (e:sPL_expr) : sPL_type option * sPL_expr =
  Debug.no_1 "type_infer" pr_none pr_none (fun _ -> type_infer_x env e) e 




(* number of arguments for full application *)
(* Ex: num_of_arg (int->(int->int)->int) ==> 2 *)
let rec num_of_arg rt =
  match rt with
    | Arrow (_,t2) -> 1+(num_of_arg t2)
    | _ -> 0

(* determine if sufficient argument for type *)
(* if insufficient - return fresh id and residual type *)
(* get_partial int->int->int [2] ===> Some (["_tmp_1"],int->int *)
(* get_partial int->int->int [] ===> Some (["_tmp_1";"_tmp_2"],int->int->int *)
let get_partial (t:sPL_type) (args:'b list) =
  if not(!pa_removal_flag) then None
  else
    match extr_arg_type t args with
      | None -> None
      | Some (ls,rt) -> 
          let narg = num_of_arg rt in
            if narg=0 then None
            else Some (rt,(names # fresh_strs "_pa_var" narg))


let rec build_type ls bt =
  match ls with
    | [] -> bt
    | (t,_,_)::ls -> Arrow(t,build_type ls bt)


(* 
preprocessing to remove 
(i) partial application 
(ii) let construct
S.sPL_expr --> C.sPL_expr
*)
let trans_exp (e:sPL_expr) : C.sPL_expr  =
  let rec aux e =
    match e with
      | BoolConst v -> C.BoolConst v
      | IntConst v -> C.IntConst v
      | Var v -> C.Var v
      | UnaryPrimApp (op,arg) ->
          let varg = aux arg in
            (C.UnaryPrimApp (op,varg))
      | BinaryPrimApp (op,arg1,arg2) ->
          let varg1 = aux arg1 in
          let varg2 = aux arg2 in
            (C.BinaryPrimApp (op,varg1,varg2))
      | Cond (e1,e2,e3) ->
          let v1 = aux e1 in
          let v2 = aux e2 in
          let v3 = aux e3 in
            C.Cond (v1,v2,v3)
      | Func (t,vs,body) ->
          let nbody = aux body in
            C.Func (t,vs,nbody)
      | RecFunc (f,t,vs,body) ->
          let nbody = aux body in
            C.RecFunc (f,t,vs,nbody)
      | Appln (f,t,args) ->
          begin
            match t with
              | Some t1 ->
                  begin
                    let args = List.map aux args in
                    let f = aux f in
                      match get_partial t1 args with
                        | None ->  C.Appln (f,t1,args)
                        | Some (t2,ns) -> C.Func(t2,ns,C.Appln(f,t1,args@(List.map (fun v -> C.Var v) ns)))
                  end
              | _ -> failwith "missing type : not possible"
          end
      | Let (ls,t,body) ->
          (* transform Let into a function application *)
          (* build a correct type for the function from *) 
          (* the type of arguments (local vars) and body *)
          let nls = (List.map (fun x -> match x with 
                                |a,b,c -> c) ls) in
          let ids = ((List.map (fun x -> match x with 
                                 |a,b,c -> b)) ls) in
          let types = ((List.map (fun x -> match x with 
                                   |a,b,c -> a)) ls) in
          let func_type = (List.fold_right (fun var_type ret_type -> Arrow (var_type, ret_type)) (List.rev types) t) in
            aux (Appln (Func (func_type,ids,body),(Some t),nls))

      (* CHANGED*)
      | Row (args) ->
        let cargs = List.map aux args in
          C.Row (cargs)
      | Relation (t, rows) ->
        let crows = List.map aux rows in
          C.Relation (t, crows)
      | Proj (projections) -> C.Proj(projections)
  in aux e

(* calling sPL parser *)
(*let parse_file (filename:string) : (string * sPL_expr) =
  SPL_parser.parse_file filename *)

(* set up for command argument
   using Sys and Arg modules *)
(* let usage = "usage: " ^ Sys.argv.(0) ^ " <filename>" *)
(* let file = ref ""  *)


(* Extra Assignment for 10% Bonus *)
(*
Currently types are given at the following 
places (or features):
(i) body of let
(ii) local definitions of let
(iii) function definition
(iv) recursive function definition
The extra assignment requires you to make their
type declaration optional. I suggest you do them 
gradually, starting with (i), then (ii) etc.
You must do the following for each:
(a) change the corresponding type of each
feature to option type in sPL.ml 
(b) change parser to make the type declaration 
optional for those features
(c) change type_infer to infer types when not given
(d) core language in sPLc.ml must have fully inferred type.
*)
