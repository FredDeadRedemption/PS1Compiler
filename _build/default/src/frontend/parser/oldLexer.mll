{
open Token
open Lexing

exception Eof
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

(* Regexes for tokens *)
let integer = "-?"digit+ (* "-?" minus is optional "digit+" digit of any length *)
let float = "-?"digit+(digit+)? (* VED IK OM DEN HER VIRKER *)
let identifier = (alpha) (alpha|digit|'_')* (* must start with alpha char. the a-z 0-9 or _ is allowed 0 or infinite times *)

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule tokenize = parse
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "." { DOT }
  | ":" { COLON }
  | ";" { SEMICOLON }
  | "=" { EQUAL }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL }
  | "/" { DIV }
  | "%" { MOD }
  | "AND" { AND }
  | "OR" { OR }
  | "!" { NEG }
  | "let" { LET }
  | "const" { CONST }
  | "function" { FUN }
  | "true" { TRUE }
  | "false" { FALSE }
  | "if" { IF }
  | "else" { ELSE }
  | "while" { WHILE }
  | "for" { FOR }
  | "main" { MAIN }
  | whitespace { tokenize lexbuf } (* skip whitespace *)
  | newline { tokenize lexbuf } (* skip newline *)
  (*| "//" { read_comment lexbuf }
  | "/*" { read_multiline_comment lexbuf }*)
  | integer as i { INT(i) }
  | eof { EOF }
  | _ {raise Eof }

{
let main () = begin
  try
    let filename = Sys.argv.(1) in
    let filehandle = open_in filename in
    let lexbuf = Lexing.from_channel filehandle in
    while true do 
      let result = tokenize lexbuf in
      match result with 
      | INT -> Printf.printf "digit\n"
      | ADD -> Printf.printf "+\n"
    done
  with Eof -> exit 0
end ;;
main () ;;
}