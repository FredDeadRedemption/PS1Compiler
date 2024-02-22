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
let float = ('-'? integer fraction?) | ('-'? fraction) (* VED IK OM DEN HER VIRKER *)
let identifier = (alpha) (alpha|digit|'_')* (* must start with alpha char. the a-z 0-9 or _ is allowed 0 or infinite times *)

let whitespace = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"

rule tokenize = parse
  | whitespace { tokenize lexbuf } (* skip whitespace *)
  | newline { tokenize lexbuf } (* skip newline *)
  (*| "//" { read_comment lexbuf }
  | "/*" { read_multiline_comment lexbuf }*)
  | integer as i { INT(int_of_string i) }
  | float as i { FLOAT(float_of_string i) }
  | "+" { ADD }
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
      | ADD -> Printf.printf "+ (PLUS)\n"
    done
  with Eof -> exit 0
end ;;
main () ;;
}