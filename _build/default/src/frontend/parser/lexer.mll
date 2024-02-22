{
open Token
(*open Lexing*)

exception Eof
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let fraction = '.' digit+

(* Regexes for tokens *)
let integer = ('-'? digit+) (* "-?" minus is optional "digit+" digit of any length *)
let float = (integer fraction?) | ('-'? fraction) (* VED IK OM DEN HER VIRKER *)
let identifier = (alpha) (alpha|digit|'_')* (* must start with alpha char. the a-z 0-9 or _ is allowed 0 or infinite times *)

let whitespace = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"

rule tokenize = parse
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "[" { LBRACK }
  | "]" { RBRACK }
  | "." { DOT }
  | ":" { COLON }
  | ";" { SEMICOLON }
  | "=" { EQ }
  | "+" { ADD }
  | "-" { SUB }
  | "*" { MUL } 
  | "/" { DIV }
  | "%" { MOD }
  | "<" { LANGLE }
  | ">" { RANGLE }
  | "&&" { AND }
  | "||" { OR }
  | "!" { NEG }
  | "let" { LET }
  | "const" {CONST }
  | "function" { FUN }
  | "int" { TYPE_INT }
  | "float" { TYPE_FLOAT }
  | "bool" { TYPE_BOOL } 
  | "void" { TYPE_VOID }
  | "null" { TYPE_NULL }
  | "true" { TRUE }
  | "false" { FALSE }
  | "while" { WHILE }
  | "if" { IF }
  | "else" { ELSE }
  | "for" { FOR }
  | "main" { MAIN }
  | "print" { PRINT } 
  | "return" { RETURN }
  | whitespace { tokenize lexbuf } (* skip whitespace *)
(*| "//" { read_comment lexbuf }
  | "/*" { read_multiline_comment lexbuf } *)
  | integer as i { INT(int_of_string i) }
  | float as i { FLOAT(float_of_string i) }
(*| identifier { read_string ID } *)
  | newline { tokenize lexbuf } (* skip newline *)
  | eof { raise Eof }
  | _ as c { failwith (Printf.sprintf "unexpected character: %C" c) }

{
let main () = begin
  try
    let filename = "../../../code.psx" in (* Sys.argv.(1) pass file as cli arg*)
    let filehandle = open_in filename in
    let lexbuf = Lexing.from_channel filehandle in
    while true do 
      let result = tokenize lexbuf in
      match result with 
      | INT(i) -> Printf.printf "%d (INT)\n" i
      | FLOAT(i) -> Printf.printf "%f (FLOAT)\n" i
      | LPAREN -> Printf.printf "( (LPAREN)\n"
      | RPAREN -> Printf.printf ") (RPAREN)\n"
      | LBRACE -> Printf.printf "{ (LBRACE)\n"
      | RBRACE -> Printf.printf "} (RBRACE)\n"
      | LBRACK -> Printf.printf "[ (LBRACK)\n"
      | RBRACK -> Printf.printf "] (RBRACK)\n"
      | DOT -> Printf.printf ". (DOT)\n"
      | COLON -> Printf.printf ": (COLON)\n"
      | SEMICOLON -> Printf.printf "; (SEMICOLON)\n"
      | EQ -> Printf.printf "= (EQ)\n"
      | ADD -> Printf.printf "+ (ADD)\n"
      | SUB -> Printf.printf "- (SUB)\n"
      | MUL -> Printf.printf "* (MUL)\n"
      | DIV -> Printf.printf "/ (DIV)\n"
      | MOD -> Printf.printf "%% (MOD)\n"
      | LANGLE -> Printf.printf "< (LANGLE)\n"
      | RANGLE -> Printf.printf "> (RANGLE)\n"
      | AND -> Printf.printf "&& (AND)\n"
      | OR -> Printf.printf "|| (OR)\n"
      | NEG -> Printf.printf "! (NEG)\n"
      | LET -> Printf.printf "let (LET)\n"
      | CONST -> Printf.printf "const (CONST)\n"
      | FUN -> Printf.printf "fun (FUN)\n"
      | TYPE_INT -> Printf.printf "int (TYPE_INT)\n"
      | TYPE_FLOAT -> Printf.printf "float (TYPE_FLOAT)\n"
      | TYPE_BOOL -> Printf.printf "bool (TYPE_BOOL)\n"
      | TYPE_VOID -> Printf.printf "void (TYPE_VOID)\n"
      | TYPE_NULL -> Printf.printf "null (TYPE_NULL)\n"
      | TRUE -> Printf.printf "true (TRUE)\n"
      | FALSE -> Printf.printf "false (FALSE)\n"
      | WHILE -> Printf.printf "while (WHILE)\n"
      | IF -> Printf.printf "if (IF)\n"
      | ELSE -> Printf.printf "else (ELSE)\n"
      | FOR -> Printf.printf "for (FOR)\n"
      | MAIN -> Printf.printf "main (MAIN)\n"
      | PRINT -> Printf.printf "print (PRINT)\n"
      | RETURN -> Printf.printf "return (RETURN\n)"
    done
  with Eof -> exit 0
end ;;
main () ;;
}