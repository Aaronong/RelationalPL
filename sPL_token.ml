open Camlp4.PreCast

type sPL_token = 
  | IDENTIFIER of string
  | INT_LIT of int * string
  | CHAR_LIT of char * string
  | STRING of string * string
  | TRUE | FALSE
  | INT_TYP | BOOL_TYP
  | PLUS | UMINUS | MINUS | STAR | DIV
  | EQ | LT | GT
  | AND | OR | NEG
  | LETWORD | INWORD | ENDWORD
  | FUN | RECFUN | RARROW
  | OPAREN | CPAREN
  | OBRACE | CBRACE
  | IFWORD | THENWORD | ELSEWORD
  (* CHANGED *)
  | EOF | RELATIONWORD | ROWWORD
  | RELATION_TYP | COLON | SEMICOLON
  | JOIN | PROJECT | PROJWORD | PROJ_TYP

module type SPLTokenS = Camlp4.Sig.Token with type t = sPL_token

module SPL_token = struct
  open Format
  module Loc = Loc
  type t = sPL_token
  type token = t

  let sf = Printf.sprintf

  let to_string k = match k with
    | IDENTIFIER s | INT_LIT(_,s) | CHAR_LIT(_,s) | STRING(_,s) -> s
    | TRUE -> "true" | FALSE -> "false"
    | INT_TYP -> "int" | BOOL_TYP -> "bool"
    | PLUS -> "+" | UMINUS -> "~" | MINUS -> "-" | STAR -> "*" | DIV -> "/"
    | EQ -> "=" | LT -> "<" | GT -> ">"
    | AND -> "&" | OR -> "|" | NEG -> "\\"
    | LETWORD -> "let" | INWORD -> "in" | ENDWORD -> "end"
    | FUN -> "fun" | RECFUN -> "recfun" | RARROW -> "->"
    | OPAREN -> "(" | CPAREN -> ")"
    | OBRACE -> "{" | CBRACE -> "}"
    | IFWORD -> "if" | THENWORD -> "then" | ELSEWORD -> "else"
    (* CHANGED *)
    | EOF -> "" | RELATIONWORD -> "relation" | ROWWORD -> "row"
    | RELATION_TYP -> "relationtype" | COLON -> ":" | SEMICOLON -> ";"
    | JOIN -> "|><|" | PROJECT -> "|||" | PROJWORD -> "projection"
    | PROJ_TYP -> "projectiontype"

  let print ppf x = pp_print_string ppf (to_string x)

  let match_keyword kwd _ = false

  let extract_string t = match t with
    | IDENTIFIER s | INT_LIT(_,s) | CHAR_LIT(_,s) | STRING(_,s) -> s
    | _ -> ""

  module Error = struct
    type t = string
    exception E of string
    let print = pp_print_string
    let to_string x = x
  end

  module Filter = struct
    type token_filter = (t,Loc.t) Camlp4.Sig.stream_filter

    type t = {is_kwd: string -> bool;
              mutable filter : token_filter}

    let mk is_kwd = {is_kwd = is_kwd;
                     filter = (fun s -> s)}

    let filter x = fun strm -> x.filter strm

    let define_filter x f = x.filter <- f x.filter

    let keyword_added _ _ _ = ()

    let keyword_removed _ _ = ()
  end

end

module Error = Camlp4.Struct.EmptyError
