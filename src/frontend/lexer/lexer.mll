{
  open Lexing
  open Parser

  (* Lexical analysis error *)
  exception Lexing_error of string

  let kwd_tbl =
    [ "print", PRINT;
      "start", START;
      ]

  let id_or_kwd =
    let h = Hashtbl.create 17 in
    List.iter (fun (s,t) -> Hashtbl.add h s t) kwd_tbl;
    fun s ->
      let s = String.lowercase_ascii s in
      try List.assoc s kwd_tbl with _ -> IDENT s

 }

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = (letter | '_') (letter | digit | '_')*
let integer = ['0'-'9']+
let space = [' ' '\t']

rule tokenize = parse
  | "//" [^ '\n']* '\n'
  | '\n'    { new_line lexbuf; tokenize lexbuf }
  | space+  { tokenize lexbuf }
  | ident as id { id_or_kwd id }
  | '+'     { PLUS }
  | '-'     { MINUS }
  | '*'     { TIMES }
  | '/'     { DIV }
  | '('     { LPAREN }
  | ')'     { RPAREN }
  | '{'     { LBRACK }
  | '}'     { RBRACK }
  | ';'     { SEMICOLON }
  | "(*"    { comment lexbuf }
  | integer as s { CST (int_of_string s) }
  | eof     { EOF }
  | _ as c  { raise (Lexing_error ("illegal character: " ^ String.make 1 c)) }

(* Note : comments are not nested in our language *)
and comment = parse
  | "*)"    { tokenize lexbuf }
  | _       { comment lexbuf }
  | eof     { raise (Lexing_error ("unterminated comment")) }
