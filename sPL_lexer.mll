{
open SPL_token
(** A signature for specialized tokens. *)
module Sig = Camlp4.Sig

module Make (Token : SPLTokenS)
= struct
  module Loc = Token.Loc
  module Token = Token

  open Lexing

  (* Error report *)
  module Error = struct

    type t =
      | Illegal_character of char
      | Illegal_escape    of string
      | Unterminated_string
      | Literal_overflow of string

    exception E of t

    open Format

    let print ppf =
      function
      | Illegal_character c -> fprintf ppf "Illegal character (%s)" (Char.escaped c)
      | Illegal_escape s -> fprintf ppf "Illegal backslash escape in string or character (%s)" s
      | Unterminated_string -> fprintf ppf "String literal not terminated"
      | Literal_overflow ty -> fprintf ppf "Integer literal exceeds the range of representable integers of type %s" ty

    let to_string x =
      let b = Buffer.create 50 in
      let () = bprintf b "%a" print x in Buffer.contents b
  end;;

  let module M = Camlp4.ErrorHandler.Register(Error) in ()

  open Error

  type context =
  { loc        : Loc.t    ;
    lexbuf     : lexbuf   ;
    buffer     : Buffer.t }

  let default_context lb =
  { loc        = Loc.ghost ;
    lexbuf     = lb        ;
    buffer     = Buffer.create 256 }

  (* To buffer string literals *)

  let store c = Buffer.add_string c.buffer (Lexing.lexeme c.lexbuf)
  let istore_char c i = Buffer.add_char c.buffer (Lexing.lexeme_char c.lexbuf i)
  let buff_contents c =
    let contents = Buffer.contents c.buffer in
    Buffer.reset c.buffer; contents

  let loc c = Loc.merge c.loc (Loc.of_lexbuf c.lexbuf)
  let set_start_p c = c.lexbuf.lex_start_p <- Loc.start_pos c.loc
  let move_start_p shift c =
    let p = c.lexbuf.lex_start_p in
    c.lexbuf.lex_start_p <- { (p) with pos_cnum = p.pos_cnum + shift }

  let update_loc c = { (c) with loc = Loc.of_lexbuf c.lexbuf }
  let with_curr_loc f c = f (update_loc c) c.lexbuf
  let parse_nested f c =   with_curr_loc f c;   set_start_p c;    buff_contents c
  let shift n c = { (c) with loc = Loc.move `both n c.loc }
  let store_parse f c = store c ; f c c.lexbuf
  let parse f c = f c c.lexbuf

  (* Update the current location with file name and line number. *)

  let update_loc c file line absolute chars =
    let lexbuf = c.lexbuf in
    let pos = lexbuf.lex_curr_p in
    let new_file = match file with
                  | None -> pos.pos_fname
                  | Some s -> s  in
    lexbuf.lex_curr_p <- { pos with
      pos_fname = new_file;
      pos_lnum = if absolute then line else pos.pos_lnum + line;
      pos_bol = pos.pos_cnum - chars;
    }

  let err error loc = raise(Loc.Exc_located(loc, Error.E error))

  let warn error loc = Format.eprintf "Warning: %a: %a@." Loc.print loc Error.print error

 let sPL_keywords = Hashtbl.create 100

 let _ = List.map (fun ((k,t):(string*sPL_token)) -> Hashtbl.add sPL_keywords k t)
	[("int", INT_TYP);
   ("bool", BOOL_TYP);
   ("true", TRUE);
   ("false", FALSE);
   ("let", LETWORD);
	 ("in", INWORD);
	 ("end", ENDWORD);
   ("fun", FUN);
   ("recfun", RECFUN);
   ("if" , IFWORD);
   ("then", THENWORD);
   ("else", ELSEWORD);
   ("relation", RELATIONWORD);
   ("row", ROWWORD);
   ("relationtype", RELATION_TYP);
   ("projection", PROJWORD);
   ("projectiontype", PROJ_TYP)
 ]
}

  let newline = ('\010' | '\013' | "\013\010")
  let blank = [' ' '\009' '\012']
  let alpha = ['a'-'z' 'A'-'Z' '\223'-'\246' '\248'-'\255' '_']
  let identchar = ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '0'-'9']
  let identseq = alpha identchar*
  let ident = (identseq | identseq ''') ('.' identseq)*
  let locname = ident
  (* CHANGED - added semicolon*)
  let not_star_symbolchar = ['$' '!' '%' '&' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~' '\\' ';']
  let symbolchar = '*' | not_star_symbolchar
  let hexa_char = ['0'-'9' 'A'-'F' 'a'-'f']
  let decimal_literal = ['0'-'9'] ['0'-'9' '_']*
  let hex_literal = '0' ['x' 'X'] hexa_char ['0'-'9' 'A'-'F' 'a'-'f' '_']*
  let oct_literal = '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
  let bin_literal = '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
  let int_literal = decimal_literal | hex_literal | oct_literal | bin_literal
  let float_literal = ['0'-'9'] ['0'-'9' '_']* ('.') ['0'-'9' '_']+  (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?

rule tokenizer file_name = parse
  | newline                            { update_loc file_name None 1 false 0; tokenizer file_name lexbuf }
  | blank+                                                  { tokenizer file_name lexbuf }
  | int_literal as i
        { try  INT_LIT(int_of_string i, i)
          with Failure _ -> err (Literal_overflow "int") (Loc.of_lexbuf lexbuf) }
  | "'\\" (_ as c)
        { err (Illegal_escape (String.make 1 c)) (Loc.of_lexbuf lexbuf)         }
  | '"'
        { with_curr_loc string file_name;
          let s = buff_contents file_name in STRING (Camlp4.Struct.Token.Eval.string s, s)     }
  | "'" (newline as x) "'"
        { update_loc file_name None 1 false 1; CHAR_LIT (Camlp4.Struct.Token.Eval.char x, x)       }
  | "'" ( [^ '\\' '\010' '\013']
            | '\\' (['\\' '"' 'n' 't' 'b' 'r' ' ' '\'']
                |['0'-'9'] ['0'-'9'] ['0'-'9']
                |'x' hexa_char hexa_char)
          as x) "'"                                { CHAR_LIT (Camlp4.Struct.Token.Eval.char x, x) }
  | '+' {PLUS}
  | '-' {MINUS}
  | '~' {UMINUS}
  | '*' {STAR}
  | '/' {DIV}
  | '=' {EQ}
  | '<' {LT}
  | '>' {GT}
  | '&' {AND}
  | '|' {OR}
  | '\\' {NEG}
  | "->" {RARROW}
  | "|><|" {JOIN}
  | "|||" {PROJECT}
  | '(' {OPAREN}
  | ')' {CPAREN}
  | '{' {OBRACE}
  | '}' {CBRACE}
  | ':' {COLON}
  | ';' {SEMICOLON}

  | ident as idstr
	  {
	    try Hashtbl.find sPL_keywords idstr
		  with | _ -> IDENTIFIER idstr
	  }
  | eof
      { let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <- { pos with pos_bol  = pos.pos_bol  + 1 ;
                                        pos_cnum = pos.pos_cnum + 1 }; EOF      }
  | _ as c                 { err (Illegal_character c) (Loc.of_lexbuf lexbuf) }

and string c = parse
      '"'                                                       { set_start_p c }
    | '\\' newline ([' ' '\t'] * as space)
        { update_loc c None 1 false (String.length space);
          store_parse string c                                                  }
    | '\\' ['\\' '"' 'n' 't' 'b' 'r' ' ' '\'']           { store_parse string c }
    | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']                 { store_parse string c }
    | '\\' 'x' hexa_char hexa_char                       { store_parse string c }
    | '\\' (_ as x)
        { begin
            warn (Illegal_escape (String.make 1 x)) (Loc.of_lexbuf lexbuf);
            store_parse string c
          end }
    | newline
      { update_loc c None 1 false 0; store_parse string c                       }
    | eof                                     { err Unterminated_string (loc c) }
    | _                                                  { store_parse string c }
{

  let lexing_store s buff max =
    let rec self n s =
      if n >= max then n
      else
        match Stream.peek s with
        | Some x ->
            Stream.junk s;
            Bytes.set buff n x;
            (* buff.[n] <- x; *)
            succ n
        | _ -> n
    in
    self 0 s

  let from_context c =
    let next _ =
      let tok = with_curr_loc tokenizer c in
      let loc = Loc.of_lexbuf c.lexbuf in
      Some ((tok, loc))
    in Stream.from next

  let from_lexbuf lb =
    let c = { (default_context lb) with  loc = Loc.of_lexbuf lb}
    in from_context c

  let setup_loc lb loc =
    let start_pos = Loc.start_pos loc in
    lb.lex_abs_pos <- start_pos.pos_cnum;
    lb.lex_curr_p  <- start_pos

  let from_string loc str =
    let lb = Lexing.from_string str in
    setup_loc lb loc;
    from_lexbuf lb

  let from_stream loc strm =
    let lb = Lexing.from_function (lexing_store strm) in
    setup_loc lb loc;
    from_lexbuf lb

  let mk () loc strm = from_stream loc strm
end
}
