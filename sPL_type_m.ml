#include "xdebug.cppo"
(* PLEASE DO NOT CHANGE THIS FILE *)
open SPL_type
open Debug
open SPLc
open SPL_inter
module S = SPL

let parse_file (filename:string) : (string * SPL.sPL_expr) =
  SPL_parser.parse_file filename

(* main program *)
let main =
  if String.length !VarGen.file == 0 then print_endline VarGen.usage else 
    let _ = print_endline "LOADING sPL program ..\n" in
    let (s,p) = parse_file !VarGen.file in
    let _ = print_endline ("  "^s) in
    let _ = print_endline (" \n\nAS ==> \n"^(S.string_of_sPL p)) in
    let _ = print_endline " \n\n\nTYPE CHECKING program .. " in
    let (v,np) = (type_infer [] p) in
      match v with
        | None -> print_endline " \n==> type error detected \n"
        | Some t ->
            begin
              print_endline (" \n==> inferred type "^(S.string_of_sPL_type t));
              let _ = print_string " \nTRANSFORMING ==> \n" in
              let np = trans_exp np in
              let _ = print_endline (string_of_sPL np) in
              let _ = print_string " \n\nInterpreting ==> \n" in
              let ip = evaluate [] np in
              let _ = print_endline (string_of_sPL ip) in
                ()
            end
