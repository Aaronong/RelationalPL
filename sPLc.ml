(* PLEASE DO NOT CHANGE THIS FILE *)

module S = SPL

type op_id = S.op_id
type id = S.id
type sPL_type = S.sPL_type

(* open Gen *)
open Debug


(* AST for core language *)
type sPL_expr =
  | BoolConst of bool
  | IntConst of int
  | Var of id
  | UnaryPrimApp of op_id * sPL_expr
  | BinaryPrimApp of op_id * sPL_expr * sPL_expr
  | Cond of sPL_expr * sPL_expr * sPL_expr
  | Func of sPL_type * (id list) * sPL_expr 
        (* min of one parameter *)
  | RecFunc of sPL_type * id * (id list) * sPL_expr 
        (* min of one parameter *)
  | Appln of sPL_expr * sPL_type * (sPL_expr list) 
        (* all applications fully applied *)

  (* CHANGED - SPL type of relation must be RelationType, sPL_expr of Relation must be Row, this must be verified on type_check *)
  | Row of sPL_expr list
  | Relation of sPL_type * (sPL_expr list)
  | Proj of id list

(* display sPL expr in prefix form *)
(* PLEASE do not change *)
let string_of_sPL (e:sPL_expr):string =
  let pr_type t = "{"^(S.string_of_sPL_type t)^"}" in
  let rec aux e =
  match e with
    | BoolConst v -> "Bool("^(string_of_bool v)^")"
    | IntConst v -> "Int("^(string_of_int v)^")"
    | Var v -> "Var("^v^")"
    | UnaryPrimApp (op,arg) -> op^"["^(aux arg)^"]"
    | BinaryPrimApp (op,arg1,arg2) -> op^"["^(aux arg1)^","^(aux arg2)^"]"
    | Cond (e1,e2,e3) -> "if "^(aux e1)^" then "^(aux e2)^" else "^(aux e3)
    | Func (t,args,body) -> "fun "^(pr_type t)^" typeend "^(pr_lst " " pr_id args)^" argsend "^(aux body)^" bodyend"
    | RecFunc (t,r,args,body) -> "recfun "^r^" "^(pr_type t)^" "^(pr_lst " " pr_id args)^" -> "^(aux body)^" recend"
    | Appln (e,t,args) -> "Appln["^(aux e)^"??? "^(pr_lst ";" aux args)^"]"

    (* CHANGED *)
    | Row (lst) -> "\n\tRow "^(pr_lst " " aux lst)^" end"
    | Relation (t, lst) -> "\nRelation "^(pr_type t)^" "^(pr_lst " " aux lst)^"\nend"
    | Proj (lst) -> "("^(pr_lst ", " pr_id lst)^")"
  in aux e


(* free vars of an expression *)
let rec fv (e:sPL_expr) : id list =
  match e with
    | BoolConst _  | IntConst _ -> []
    | Var i -> [i]
    | UnaryPrimApp (_,arg) -> fv arg
    | BinaryPrimApp (_,arg1,arg2) -> (fv arg1)@(fv arg2)
    | Cond (e1,e2,e3) -> (fv e1)@(fv e2)@(fv e3)
    | Func (_,vs,body) -> diff (fv body) vs
    | RecFunc (_,i,vs,body) -> diff (fv body) (i::vs)
    | Appln (e1,_,es) -> (fv e1)@(List.concat (List.map fv es))

    (* CHANGED *)
    | Row (lst) -> List.concat (List.map fv lst)
    | Relation (_, lst) -> List.concat (List.map fv lst)
    | Proj (lst) -> lst


