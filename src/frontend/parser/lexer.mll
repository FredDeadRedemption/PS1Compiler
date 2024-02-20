{

}
(* Helper regexes *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

(* Regexes for tokens *)
let int = '-'? digit+
let id = (alpha) (alpha|digit|'_')*
let generic_type_param =  ['A' -'Z']

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule token = parse
| eof {}