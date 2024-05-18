{
  open Parser

  let line = ref 1 (* ref = mutable*)

  let increase () =
    line := !line + 1 (* := used for assigning mutables ! is derefrence operator*)
}

(* regex helpers *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let lilAlpha = ['a'-'z']
let bigAlpha = ['A'-'z']
let fraction = '.' digit+ (* måske lave float uden decimaltal *)

(* regexes for tokens *)
let integer = ('-'? digit+) (* "-?" minus is optional "digit+" digit of any length *)
let float = (integer) (fraction) 
let string = '"' ([^ '"' '\\'] | '\\')* '"' (* [^ '"' '\\'] Any character that is not a double quote '"', a backslash, or a single quote *)
let identifier = (lilAlpha|'_') (alpha|digit|'_')* (* must start with alpha char. the a-z 0-9 or _ is allowed 0 or infinite times *)
let generic = (bigAlpha) (alpha|digit|'_')*

let whitespace = [' ' '\t']
let newline = '\r' | '\n' | "\r\n"

rule tokenize = parse
  | whitespace { tokenize lexbuf } (* skip whitespace *)
  | newline    { increase(); tokenize lexbuf } (* skip newline *)
  | '+'        { ADD }
  | '-'        { SUB }
  | '*'        { MUL }
  | '/'        { DIV }
  | "%"        { MOD }
  | "="        { EQ }
  | "!"        { EXCL }
  | "&&"       { AND }
  | "||"       { OR }
  | ">"        { RANGLE }
  | "<"        { LANGLE }
  | '('        { LPAREN }
  | ')'        { RPAREN }
  | '['        { LSQBRACK }
  | ']'        { RSQBRACK }
  | '{'        { LCURBRACK }
  | '}'        { RCURBRACK }
  | ','        { COMMA }
  | ';'        { SEMICOLON }
  | ':'        { COLON }
  | '.'        { DOT }
  | "return"   { RETURN }
  | "break"    { BREAK }
  | "continue" { CONTINUE }
  | "if"       { IF }
  | "else"     { ELSE }
  | "start"    { START }
  | "update"   { UPDATE }
  | "true"     { TRUE }
  | "false"    { FALSE }
  | "int"      { TYPE_INT }
  | "float"    { TYPE_FLOAT }
  | "bool"     { TYPE_BOOL }
  | "void"     { TYPE_VOID }
  | "class"         { CLASS }
  | "new"           { NEW }
  | "this" as s     { THIS s }
  | "super" as s    { SUPER s }
  | "++"       { INCR }
  | "--"       { DECR }
  | "+="       { INCRBYVAL}
  | "-="       { DECRBYVAL}
  | "GameObject" as s { GAMEOBJECT s }
  | "//" { read_comment lexbuf }
  | "/*" { read_multi_line_comment lexbuf } 
  | integer as i { INT (int_of_string i) }
  | float as f { FLOAT (float_of_string f) }
  | identifier as s { ID s }
  | generic as g { TYPE_GENERIC g }
  | eof { EOF }
  | _ as c { failwith (Printf.sprintf "Syntax error: unexpected character: %C, on line: %d" c !line) }

and read_comment = parse
  | newline { increase(); tokenize lexbuf } (* go back to tokenization on newline *)
  | eof { EOF }
  | _ { read_comment lexbuf } (* read all kinds of chars within comment *)

and read_multi_line_comment = parse
  | "*/" { tokenize lexbuf } (* go back to tokenization on */ *)
  | newline { increase(); read_multi_line_comment lexbuf } 
  | eof { failwith (Printf.sprintf "Syntax error: unexpected eof, please terminate comment on line: %d" !line) }
  | _ { read_multi_line_comment lexbuf } (* read all kinds of chars within comment *)
