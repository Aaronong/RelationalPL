open SPLc
open SPL_type
open SPL_parser
open Debug

type env_val = sPL_expr Environ.et
let get_value env v = Environ.get_val env v


(* use primitive rule to contract op[v1,v2] or op[v] to a value *)
(* raise an exception if we cannot directly contract *)

let rec find_left x lst =
    match lst with
    | [] -> raise (Failure "Not Found")
    | h :: t -> if x = h then 0 else 1 + find_left x t

let rec find_right x lst =
  ((List.length lst) - 1 - (find_left x (List.rev lst)))

let get_cartesian_rows (l1:sPL_expr list) (l2:sPL_expr list) : sPL_expr list =
  let rec helper a b acc =
  match a with 
    |Row h::t -> helper (t) b ((List.map (fun (Row bling) -> Row (h@bling) ) b)@acc)
    | [] -> acc
    | _ -> acc
  in helper l1 l2 []

let identical_columns (t:SPLc.S.sPL_type) : (int * int) list =
  let rec helper (typ:SPLc.S.sPL_type) acc left_index =
    match typ, t with
      | RelationType (x::tups), RelationType (kk) -> 
          let right_index = (List.length kk) - 1 - (find_left x (List.rev(x::tups))) in
          if (left_index = right_index) then helper (RelationType(tups)) acc (left_index+1) else
          helper (RelationType(tups)) ((left_index,right_index)::acc) (left_index+1)
      | _ -> acc
  in helper t [] 0

let remove_bad_match (e:sPL_expr) : sPL_expr =
  match e with
    | Relation (t, rows) ->
      begin
        BoolConst false
      end

let get_valid_rows (rows: sPL_expr list) (sim_col: (int * int) list) : sPL_expr list =
  let rec helper col (pr_rows: sPL_expr list)=
    match col with
      | (left,right)::t -> helper t (List.filter (fun (Row stuf) -> (List.nth stuf left) = (List.nth stuf right) ) pr_rows)
      | [] -> pr_rows
  in helper sim_col rows

let del_n lst (n:int) =
  let rec helper curr_n processed_lst =
    match processed_lst with
      | h::t -> if curr_n = n then (helper (curr_n+1) t) else h::(helper (curr_n+1) t)
      | [] -> processed_lst
  in helper 0 lst

let rm_cols (table: sPL_expr) (cols: int list) : sPL_expr =
  let rec helper curr_col pr_table =
    match pr_table, curr_col with
      | Relation (RelationType rt, rows), h::t ->
          begin
            helper (t) (Relation(RelationType (del_n rt h), List.map(fun (Row k) -> (Row (del_n k h))) rows))
          end
      | _ -> pr_table
  in helper (List.stable_sort (fun a b -> b-a ) cols) table

let rm_dup (table:sPL_expr) : sPL_expr =
  let cons_uniq xs x = if List.mem x xs then xs else x :: xs in
  let remove_from_right xs = List.rev (List.fold_left cons_uniq [] xs) in
    match table with
        | Relation (RelationType rt, rows) -> Relation (RelationType rt, remove_from_right rows)
        | _ -> table

let get_good_rows headers ids : int list =
  let head_ids = List.map (fun (a,b) -> a) headers in
  let rec helper id_list acc =
    match id_list with
      |h::t -> 
          begin
            helper t (find_left h head_ids::acc)
          end
      |_ -> acc
  in helper ids []

let get_bad_rows headers ids : int list =
  let gr = get_good_rows headers ids in
  let rec helper counter acc =
    if counter < 0 then acc else
      if List.exists (fun x -> x=counter) gr then helper (counter-1) acc else helper (counter-1) (counter::acc)
  in helper (List.length headers) []

let rec contract (e:sPL_expr): sPL_expr = 
  match e with
    | BoolConst _ | IntConst _ -> e
    | Var v -> 
          if (String.get v 0) = '~' && (String.get v ((String.length v) - 1)) = '~'
            then e else failwith ("unable to contract variable. Shouldnt happen.")
    | UnaryPrimApp (op,arg) ->
          begin
          match op with
            | "~" ->
                  begin
                  match arg with
                    | IntConst v -> IntConst (-v)
                    | _ -> failwith ("unable to contract for "^(string_of_sPL e))
                  end
            | "\\" ->
                  (* please complete *)
                  begin
                  match arg with
                    | _ -> failwith ("unable to contract for "^(string_of_sPL e))
                  end
            | _ -> failwith ("illegal unary op "^op)
          end
    | BinaryPrimApp (op,arg1,arg2) ->
          begin
          match op with
            | "+" ->
                  begin
                  match arg1,arg2 with
                    | IntConst v1,IntConst v2 -> IntConst (v1+v2)
                    | Var _, _ -> e
                    | _, Var _ -> e
                    | _,_ -> failwith ("unable to contract "^(string_of_sPL e))
                  end
            | "-" ->
                  begin
                  match arg1,arg2 with
                    | IntConst v1,IntConst v2 -> IntConst (v1-v2)
                    | Var _, _ -> e
                    | _, Var _ -> e
                    | _,_ -> failwith ("unable to contract"^(string_of_sPL e))
                  end
            | "*" ->
                  begin
                  match arg1,arg2 with
                    | IntConst v1,IntConst v2 -> IntConst (v1*v2)
                    | Var _, _ -> e
                    | _, Var _ -> e
                    | _,_ -> failwith ("unable to contract"^(string_of_sPL e))
                  end
            | "/" ->
                  begin
                  match arg1,arg2 with
                    | IntConst v1,IntConst v2 -> IntConst (v1/v2)
                    | Var _, _ -> e
                    | _, Var _ -> e
                    | _,_ -> failwith ("unable to contract"^(string_of_sPL e))
                  end
            | "|" ->
                  begin
                  match arg1,arg2 with
                    | BoolConst b1, BoolConst b2 -> BoolConst (b1 || b2)
                    | Var _, _ -> e
                    | _, Var _ -> e
                    | _,_ -> failwith ("unable to contract"^(string_of_sPL e))
                  end
            | "&" ->
                  begin
                  match arg1,arg2 with
                    | BoolConst b1, BoolConst b2 -> BoolConst (b1 && b2)
                    | Var _, _ -> e
                    | _, Var _ -> e
                    | _,_ -> failwith ("unable to contract"^(string_of_sPL e))
                  end
            | "<" ->
                  begin
                  match arg1,arg2 with
                    | IntConst v1,IntConst v2 -> BoolConst (v1 < v2)
                    | Var _, _ -> e
                    | _, Var _ -> e
                    | _,_ -> failwith ("unable to contract"^(string_of_sPL e))
                  end
            | ">" ->
                  begin
                  match arg1,arg2 with
                    | IntConst v1,IntConst v2 -> BoolConst (v1>v2)
                    | Var _, _ -> e
                    | _, Var _ -> e
                    | _,_ -> failwith ("unable to contract"^(string_of_sPL e))
                  end
            | "=" ->
                  begin
                  match arg1,arg2 with
                    | IntConst v1,IntConst v2 -> BoolConst (v1=v2)
                    | BoolConst b1, BoolConst b2 -> BoolConst (b1=b2)
                    | Var _, _ -> e
                    | _, Var _ -> e
                    | _,_ -> failwith ("unable to contract"^(string_of_sPL e))
                  end
            | "|><|" ->
                  begin
                    match arg1, arg2 with
                      | Relation (RelationType t1, rows1), Relation (RelationType t2, rows2) ->
                            let cartesian_rows = (get_cartesian_rows rows1 rows2) in
                            (* let cartesian_product = Relation( RelationType(t1@t2), cartesian_rows) in *)
                            let similar_colums = identical_columns (RelationType (t1@t2)) in
                            (* let _ = print_string ("SIM COLUMNS"^(pr_lst " " (fun (a,b) -> "("^(string_of_int a)^", "^(string_of_int b)^")") similar_colums)) in  *)
                            let valid_rows = get_valid_rows cartesian_rows similar_colums in
                            let final_relation = rm_cols (Relation( RelationType(t1@t2), valid_rows)) (List.map (fun (l,r) -> r) similar_colums) in
                            rm_dup final_relation
                      | _,_ -> failwith ("unable to contract"^(string_of_sPL e))
                  end
            | "|||" ->
                  begin
                    match arg1, arg2 with
                      | Relation (RelationType t1, rows1), Proj (ids) ->
                          begin
                            let bad_rows = get_bad_rows t1 ids in
                              rm_dup (rm_cols arg1 bad_rows)

                          end
                      | _,_ -> failwith ("unable to contract"^(string_of_sPL e))
                  end
            | _ -> failwith ("illegal binary op "^op)
          end
    | Cond (BoolConst arg1,arg2,arg3) ->
          (* In contract decide on arg1, and output arg2 or arg3 *)
          if arg1 then arg2 else arg3

    | Func (te, args, body) ->
          (* In contract output body, make sure body is irreducible *)
          body

    | RecFunc (te, s_arg, args, body) ->
          (* In contract output body, make sure body is irreducible *)
          body

    | Appln (head, t , parameters) ->
          (* In Contract output head, make sure head is irreducible *)
          head

    | _ -> failwith ("this should not happen... Contract match error")

(* Given an environment returns a unused variable *)
let new_var (env:env_val) : id =
  let rec helper curr_count =
    if (List.exists (fun (i, _) -> i="new_var"^(string_of_int curr_count)) env) then helper (curr_count+1)
    else "@new_var"^(string_of_int curr_count)
in helper 1

let rec extend_env (env:env_val) (el:sPL_expr list) : env_val =
  match el with
    | e::t -> extend_env (((new_var env),e)::env) (t)
    | [] -> env

let str_contains s1 s2 =
    let re = Str.regexp_string s2
    in
        try ignore (Str.search_forward re s1 0); true
        with Not_found -> false

let rec replace_one (env:env_val) (arg:id) : env_val =
  match env with
    | (k,v):: t -> if (str_contains k "@new_var") then ((arg,v)::t)
        else (k,v)::(replace_one (t) arg)
    | _ -> env

let rec replace_new_var (env:env_val) (args:id list) : env_val =
  match args with
    | h::t -> replace_new_var (replace_one env h) t
    | [] -> env

(* check if an expression is reducible or irreducible *)
let rec reducible (env:env_val) (e:sPL_expr) : bool = 
  match e with
    | BoolConst _ | IntConst _  | Proj _ -> false
    | BinaryPrimApp(_,e1,e2) -> 
      begin
      match e1, e2 with
        | IntConst _ , IntConst _ -> true
        | BoolConst _, BoolConst _ -> true
        | Relation _, _ -> true
        | _ -> reducible env e1 || reducible env e2
      end
    | Row stuf -> List.exists (fun x -> reducible env x) stuf
    | Relation (_, rows) -> List.exists (fun x -> reducible env x) rows
    | Var v -> if (String.get v 0) = '~' && (String.get v ((String.length v) - 1)) = '~'
                then false else true
    | Appln (head, t, parameters) -> true
    | Func (_,_,body) -> true
    | _ -> true

(* if expr is irreducible, returns it *)
(* otherwise, perform a one-step reduction *)
let rec oneStep (env:env_val) (e:sPL_expr): sPL_expr = 
  match e with
    | BoolConst _ | IntConst _ ->  e
    | Var v ->
          (match get_value env v with
            | Some v2 -> v2
            | None -> Var ("~"^v^"~")
          )
    | UnaryPrimApp (op,arg) ->
          if reducible env arg then UnaryPrimApp(op,oneStep env arg)
          else contract e
    | BinaryPrimApp (op,arg1,arg2) -> 
         if reducible env arg1 
          then BinaryPrimApp(op,oneStep env arg1,arg2)
          else 
            if reducible env arg2
            then BinaryPrimApp(op,arg1,oneStep env arg2)
            else contract e
    | Cond (arg1,arg2,arg3) ->
         if reducible env arg1 
          then Cond(oneStep env arg1, arg2, arg3)
          else 
            if reducible env arg2
            then Cond(arg1,oneStep env arg2, arg3)
            else 
              if reducible env arg3
              then Cond(arg1,arg2, oneStep env arg3)
              (* In contract decide on arg1, and output arg2 or arg3*)
              else contract e
    | Func (te, args, body) ->
          let env = List.rev (replace_new_var (List.rev env) args) in
          if reducible env body then Func(te, args, oneStep env body)
          (* In contract output arg3, make sure arg3 irreducible *)
          else contract e
    | RecFunc (te, s_arg, args, body) ->
          let env = ((s_arg,e)::(List.rev (replace_new_var (List.rev env) args))) in
          if reducible env body then RecFunc(te, s_arg, args, oneStep env body)
          (* In contract output arg3, make sure arg3 irreducible *)
          else contract e
    | Appln (head, t , parameters) ->
          (* Store params in list. with placeholder values replace placeholders in func *)
          if (List.exists (fun x -> reducible env x) parameters) then Appln (head, t, (List.map (fun p -> if (reducible env p) then (oneStep env p) else contract p) parameters)) else
            let env = extend_env env parameters
            in 
              (* In Contract output head, make sure head is irreducible *)
              if reducible (env) head then Appln (oneStep (env) head, t, parameters) else contract e

    | Proj _ -> e

    | Row stuf ->
        if reducible env e then Row (List.map (fun item -> oneStep env item) stuf)
      else e
    | Relation (t, rows) ->
        if reducible env e then Relation (t, List.map (fun row -> oneStep env row) rows)
      else e



(* keep reducing until we get a irreducible expr *)
(* or has an exception due to wrong operator or type error *)
let rec evaluate (env:env_val) (e:sPL_expr): sPL_expr = 
  if (reducible env e) then evaluate env (oneStep env e)
  else e


(* sample expr in AST form *)
let e1 = IntConst 42
let e2 = 
  BinaryPrimApp ("+",
    BinaryPrimApp("*",
      UnaryPrimApp("~",IntConst 15),
      IntConst 7),
    IntConst 2)
let e2a = 
  BinaryPrimApp (">",IntConst 7,IntConst 10)
let e2b = 
  BinaryPrimApp ("=",
    IntConst 10,
    BinaryPrimApp("+",IntConst 3,IntConst 7))

let e3 = 
  BinaryPrimApp ("|",
    BinaryPrimApp("&",
      UnaryPrimApp("\\",BoolConst false),
      BoolConst true),
    BoolConst true)
let e4 = 
  BinaryPrimApp ("+",IntConst 15,BoolConst true)
let e5 = 
  BinaryPrimApp ("!",IntConst 15,BoolConst true)
let e5 = 
  BinaryPrimApp (">",BoolConst false,BoolConst true)
let e6 = 
  BinaryPrimApp ("*",
     BinaryPrimApp ("+",IntConst 1,IntConst 2),
     IntConst 3)
let e7 = 
  BinaryPrimApp ("+",
     IntConst 1,
     BinaryPrimApp ("*",IntConst 2,IntConst 3))


(* test driver for evaluation *)
(* let testCommand e =
  print_endline ("sPL expr:"^(string_of_sPL e));
  print_endline ("oneStep :"^(string_of_sPL (oneStep e)));
  print_endline ("evaluate:"^(string_of_sPL (evaluate e)))
 *)
(* test driver for type inference *)
(* let testType e =
  (* let s = (string_of_sPL e) in *)
  let v = (type_infer [] e) in
  match v with 
    | Some t -> print_endline ("  inferred type : "^(string_of_sPL_type t));
    | None -> print_endline ("  type error ") *)

(* let _ = testCommand e2  *)
(* let _ = testCommand e2a  *)
(* let _ = testCommand e2b  *)
(* let _ = testCommand e3 *)

(* let _ = testType e1 *)
(* let _ = testType e2 *)
(* let _ = testType e2a  *)
(* let _ = testType e2b  *)
(* let _ = testType e3 *)
(* let _ = testType e4 *)
(* let _ = testType e5 *)

(* let _ = testType e6 *)
(* let _ = testCommand e6 *)
(* let _ = testType e7 *)
(* let _ = testCommand e7 *)
(* let _ = testType e4 *)
(* let _ = testCommand e4 *)


(*
(* calling sPL parser *)
let parse_file (filename:string) : (string * sPL_expr) =
  sPL_parser.parse_file filename
 
(* set up for command argument
   using Sys and Arg modules *)
let usage = "usage: " ^ Sys.argv.(0) ^ " <filename>"
let file = ref "" 


let testType2 e =
  (* let s = (string_of_sPL e) in *)
  let v = type_infer e in
  (match v with 
    | Some t -> print_endline ("  inferred type : "^(string_of_sPL_type t));
    | None -> print_endline ("  type error!! "));v

(* main program *)
let main =
  (* Read the arguments of command *)
  Arg.parse [] (fun s -> file := s) usage; 
  if String.length !file == 0 then print_endline usage 
  else 
    let _ = print_endline "Loading sPL program .." in
    let (s,p) = parse_file !file in
    let _ = print_endline ("  "^s) in
    let _ = print_endline ("  as "^(string_of_sPL p)) in
    let _ = print_endline "Type checking program .." in
    let v = testType2 p in
    if v=None then ()
    else
      let _ = print_string "Evaluating ==> " in
      let r = evaluate p in
      print_endline (string_of_sPL r)

*)