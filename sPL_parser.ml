open Camlp4
open SPL
open SPL_token

module M = SPL_lexer.Make(SPL_token)

(* type op_id = string *)

(* (\* abstract syntax tree for ePL *\) *)
(* type ePL_expr = *)
(*   | BoolConst of bool *)
(*   | IntConst of int *)
(*   | UnaryPrimApp of op_id * ePL_expr *)
(*   | BinaryPrimApp of op_id * ePL_expr * ePL_expr *)

(* (\* display ePL expr in prefix form *\) *)
(* let rec string_of_ePL (e:ePL_expr):string = *)
(*   match e with *)
(*     | BoolConst v -> "BoolConst("^(string_of_bool v)^")" *)
(*     | IntConst v -> "IntConst("^(string_of_int v)^")" *)
(*     | UnaryPrimApp (op,arg) -> op^(string_of_ePL arg) *)
(*     | BinaryPrimApp (op,arg1,arg2) -> op^"["^(string_of_ePL arg1)^","^(string_of_ePL arg2)^"]" *)

(*
Concrete syntax:
  type = Bool | Int 
         | type "->" type
         | "(" type ")"
  args =  v | v " " args
  ldecl = (type v "=" expr)+
  expr = "let" ldecl "in" type expr "end"
       | expr expr1 .. exprn
       | "(" expr ")"
       | "fun" type args "->" expr "end"
       | "recfun" v type args "->" expr "end"
*)

module SPLGram = Camlp4.Struct.Grammar.Static.Make(SPL_lexer.Make(SPL_token))

let exp = SPLGram.Entry.mk "exp"

let peek_typ = 
 SPLGram.Entry.of_parser "peek_typ" 
     (fun strm ->
       match Stream.npeek 2 strm with
          | [OPAREN,_;OPAREN,_] | [OPAREN,_;BOOL_TYP,_] | [OPAREN,_;INT_TYP,_] -> ()
          | _ -> raise Stream.Failure)

(* convert binary application to vector application *)
let rec flatten_appln e acc =
  match e with
    | Appln (f,_,arg) -> flatten_appln f (arg@acc)
    | _ -> Appln(e,None,acc)

EXTEND SPLGram
  GLOBAL: exp;
  exp: [[e = expr;`EOF -> e]];
 
  typ: [[`BOOL_TYP -> BoolType
        | `INT_TYP -> IntType]
        | RIGHTA
          [t1=SELF;`RARROW;t2=SELF -> Arrow(t1,t2)]
        | [peek_typ;`OPAREN;t=SELF; `CPAREN -> t
        (* CHANGED *)
        | `RELATION_TYP; tuples = LIST1 tup; `ENDWORD -> RelationType (tuples)
        | `PROJ_TYP -> ProjType
  ]];

  args: [[ al = LIST1 [`IDENTIFIER s -> s] -> al ]];

  decl: [[`OBRACE;t=typ;`CBRACE; `IDENTIFIER s; `EQ; e = expr -> (t,s,e) ]];

  ldecl: [[ld = LIST1 decl -> ld]];

  (* CHANGED *)
  tup: [[`IDENTIFIER s; `COLON; t=typ;`SEMICOLON -> (s,t) ]];

  elem: [[e=expr; `SEMICOLON -> e ]];

  expr:
     [ 
      (* CHANGED *)
      "Relation" NONA
          [ `ROWWORD; e = LIST1 elem; `ENDWORD -> Row(e)
          | `RELATIONWORD; `OBRACE; t = typ; `CBRACE; r = LIST1 SELF; `ENDWORD -> Relation(t,r)
          | `PROJWORD; `OPAREN; al = args; `CPAREN -> Proj(al)]
        | "Join Project" LEFTA
            [ e1 = SELF; `JOIN; e2 = SELF -> BinaryPrimApp ("|><|",e1,e2)
            | e1 = SELF; `PROJECT; e2 = SELF -> BinaryPrimApp ("|||",e1,e2)]

      |LEFTA
          [e1 = SELF; e2 = SELF -> flatten_appln e1 [e2]]
      
       |   "Equality" LEFTA 
          [e1 = SELF; `EQ;e2 = SELF -> BinaryPrimApp ("=",e1,e2)]
        |  "Compare" LEFTA
           [e1 = SELF; `LT;e2 = SELF -> BinaryPrimApp ("<",e1,e2)
             |   e1 = SELF;  `GT;e2 = SELF -> BinaryPrimApp (">",e1,e2)]
        |  "Sub Add" LEFTA
            [ e1 = SELF;`MINUS ;  e2 = SELF -> BinaryPrimApp ("-",e1,e2)
            | e1 = SELF; `PLUS; e2 = SELF -> BinaryPrimApp ("+",e1,e2) ]
        | "Mul Div" LEFTA
            [ e1 = SELF; `STAR; e2 = SELF -> BinaryPrimApp ("*",e1,e2) 
            | e1 = SELF; `DIV;  e2 = SELF -> BinaryPrimApp ("/",e1,e2) ]
        | "And Or" LEFTA
            [ e1 = SELF; `AND; e2 = SELF -> BinaryPrimApp("&",e1,e2)
            | e1 = SELF; `OR; e2 = SELF -> BinaryPrimApp("|",e1,e2)]

         | "Bracket" LEFTA
           ["("; e=SELF; ")" -> e]



         | "Unary" NONA
           [ `UMINUS; e=SELF -> UnaryPrimApp("~",e)
            | `NEG; e=SELF -> UnaryPrimApp("\\",e)
           ]
       (* | "Boolean" NONA  *)
       (*      [e1 = SELF; `EQ ; e2 = SELF -> BinaryPrimApp ("=",e1,e2)] *)
       (*    | [e1 = SELF; `LT ; e2 = SELF -> BinaryPrimApp ("<",e1,e2) *)
       (*    |  e1 = SELF; `GT ; e2 = SELF -> BinaryPrimApp (">",e1,e2)] *)
       (*    | [e1 = SELF; `AND ; e2 = SELF -> BinaryPrimApp ("&",e1,e2) *)
       (*    |  e1 = SELF; `OR ; e2 = SELF -> BinaryPrimApp ("|",e1,e2) *)
       (*    ] *)
       (*  | "Unary" NONA *)
       (*    [ `UMINUS ; e=SELF -> UnaryPrimApp("~",e) *)
       (*    | `NEG ; e=SELF -> UnaryPrimApp("\\",e) *)
       (*    ] *)
       (*  |  "Sub" LEFTA *)
       (*    [ e1 = SELF; `MINUS ;  e2 = SELF -> BinaryPrimApp ("-",e1,e2) ] *)
       (*  | "Add" LEFTA *)
       (*    [ e1 = SELF; `PLUS ; e2 = SELF -> BinaryPrimApp ("+",e1,e2) ] *)
       (*  | "Mul" LEFTA *)
       (*    [ e1 = SELF; `STAR ; e2 = SELF -> BinaryPrimApp ("*",e1,e2) ] *)
       (*  |  "Div" LEFTA *)
       (*    [e1 = SELF; `DIV ;  e2 = SELF -> BinaryPrimApp ("/",e1,e2) ] *)
        | [`LETWORD; ld = ldecl ; `INWORD ; `OBRACE;t = typ; `CBRACE;e = SELF ; `ENDWORD -> Let(ld,t,e) 
        |  `FUN; `OBRACE;t =typ ;`CBRACE; al = args;`RARROW; e = SELF; `ENDWORD -> Func(t,al,e)
        |  `RECFUN; `IDENTIFIER s; `OBRACE;t = typ;`CBRACE; al = args; `RARROW; e = SELF; `ENDWORD 
          -> RecFunc(t,s,al,e)
        (* |  e = SELF; t = OPT typ; el = LIST1 SELF -> Appln(e,t,el) *)
          ]
        | LEFTA 
            [`IFWORD; e1 = SELF; `THENWORD; e2 = SELF; `ELSEWORD; e3 = SELF;`ENDWORD -> Cond(e1,e2,e3)]
        | "Bracket" LEFTA
           [ `OPAREN; e=SELF; `CPAREN -> e]
        | NONA 
          [ `INT_LIT (i, _) -> IntConst(i)
          | `TRUE -> BoolConst(true)
          | `FALSE -> BoolConst(false)
          | `IDENTIFIER s -> Var(s) 
          ]
      ];
  END
;;
let _loc = PreCast.Loc.mk "<string>" 

(* parse from a string *)
let parse fname str =
try 
  let e = SPLGram.parse_string exp _loc str in e 
with
      End_of_file -> exit 0
    | M.Loc.Exc_located (l,t)->
      (print_string ("Parse Error in "^(Camlp4.PreCast.Loc.to_string l)^"\n");
       raise t)

(* let main = *)
(*    print_string "# "; *)
(*   let str = read_line () in *)
(*   let e = parse str in		 *)
(*   print_string (string_of_sPL e) *)

(* parse from a file *)
let parse_file (filename:string) : (string * sPL_expr) =
  let contents =
    let lines = ref [] in
    let chan = open_in filename in
    try
      while true; do
        lines := input_line chan :: !lines
      done; []
    with End_of_file ->
        close_in chan;List.rev !lines in
  let str = String.concat "" contents in
  let chan = open_in filename in
  let istream = Stream.of_channel chan in
  try 
    let e = SPLGram.parse exp (PreCast.Loc.mk filename) istream in
    (str,e)
  with M.Loc.Exc_located (l,t)->
      (print_string ("Parse Error in "^(Camlp4.PreCast.Loc.to_string l)^"\n");
       raise t)

