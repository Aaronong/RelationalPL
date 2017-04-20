ocamlc -annot -c VarGen.ml 
ocamlc -annot -c debug.ml 
ocamlc -annot sPL.ml -a -o sPL.cma
ocamlc -annot sPLc.ml -a -o sPLc.cma
ocamlc -annot -I +camlp4 dynlink.cma camlp4lib.cma -pp camlp4of.opt sPL_token.ml -a -o sPL_token.cma 
ocamllex sPL_lexer.mll
ocamlc -annot -I +camlp4 dynlink.cma camlp4lib.cma -pp camlp4of.opt sPL_lexer.ml -a -o sPL_lexer.cma
ocamlc -annot -I +camlp4 dynlink.cma camlp4lib.cma -pp camlp4of.opt sPL_parser.ml -a -o sPL_parser.cma
ocamlc -pp "cppo -I ../ -D TRACE" -annot sPL_type.ml -a -o sPL_type.cma
ocamlc -annot sPL_inter.ml -w "-40-8" -a -o sPL_inter.cma
ocamlc -pp "cppo -I ../ -D TRACE" -annot VarGen.cmo str.cma debug.cmo  sPL.cma sPLc.cma sPL_inter.cma sPL_token.cma sPL_lexer.cmo sPL_parser.cmo sPL_type.cma sPL_type_m.ml -o splt
