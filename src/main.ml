
open Lexer
open Ast

(*let main () = begin
  try
    let filename = Sys.argv.(1) in (* Sys.argv.(1) pass file as cli arg "../../../code.psx" *)
    let filehandle = open_in filename in
    let stream = Lexing.from_channel filehandle in
    while true do 
      let result = Lexer.tokenize stream in
      match result with 
      | INT(i) -> Printf.printf "%d (INT)\n" i
      | FLOAT(i) -> Printf.printf "%f (FLOAT)\n" i
      | STRING(s) -> Printf.printf "%s (STRING)\n" s;
      | ID(s) -> Printf.printf "%s (ID)\n" s;
      | LPAREN -> Printf.printf "( (LPAREN)\n"
      | RPAREN -> Printf.printf ") (RPAREN)\n"
      | LBRACE -> Printf.printf "{ (LBRACE)\n"
      | RBRACE -> Printf.printf "} (RBRACE)\n"
      | LBRACK -> Printf.printf "[ (LBRACK)\n"
      | RBRACK -> Printf.printf "] (RBRACK)\n"
      | DOT -> Printf.printf ". (DOT)\n"
      | COMMA -> Printf.printf ", (COMMA)\n"
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
      | NOT -> Printf.printf "! (NOT)\n"
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
      | RETURN -> Printf.printf "return (RETURN)\n"
    done
  with Eof ->
    Printf.printf "File was %d lines\n" !line; 
    exit 0
end ;;
main () ;;*)

let main () = begin
  let filename = Sys.argv.(1) in (* Sys.argv.(1) pass file as cli arg "../../../code.psx" *)
    let filehandle = open_in filename in
  let lexbuf = Lexing.from_channel filehandle in
  let expr = Parser.program Lexer.tokenize lexbuf in
    print_expr expr   
end ;;
main () ;;



let rec string_of_expr = function
  | Econst n -> string_of_int n
  | Ebinop (op, e1, e2) ->
    let op_str = match op with
      | Add -> "+"
      | Sub -> "-"
      | Mul -> "*"
      | Div -> "/"
      | Lesser -> "<"
      | Greater -> ">"
      | Mod -> "%"
      | And -> "&&"
      | Or -> "||"
      | Equal -> "=="
    in
    "(" ^ string_of_expr e1 ^ " " ^ op_str ^ " " ^ string_of_expr e2 ^ ")"

let print_expr expr =
  print_endline (string_of_expr expr)

(*
(* ast.ml *)
type expr =
  | Int of int
  | Add of expr * expr
  | Sub of expr * expr
  (* Add more constructors as needed *)

(* parser.mly *)
%{
open Ast
%}

%token <int> INT
%token PLUS MINUS
%left PLUS MINUS

%start expr
%%

expr:
  INT  { Int($1) }
| expr PLUS expr { Add($1, $3) }
| expr MINUS expr { Sub($1, $3) }
;

(* ast_printer.ml *)
open Ast

let rec string_of_expr = function
  | Int n -> string_of_int n
  | Add (e1, e2) -> "(" ^ string_of_expr e1 ^ " + " ^ string_of_expr e2 ^ ")"
  | Sub (e1, e2) -> "(" ^ string_of_expr e1 ^ " - " ^ string_of_expr e2 ^ ")"
  (* Add more cases as needed *)

let print_expr expr =
  print_endline (string_of_expr expr)

(* main.ml *)
open Parser
open Lexer
open Ast_printer

let () =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Lexer.token lexbuf in
  print_expr expr   



*)
(*
(* ast.ml *)
type binop = Add | Sub | Mul | Div | Lesser | Greater | Mod | And | Or | Equal

type expr =
  | Econst of int
  | Evar   of string
  | Ebool  of bool
  | Ebinop of binop * expr * expr

(* parser.mly *)
%{
open Ast
%}

%token <int> INT
%token <string> VAR
%token TRUE FALSE
%token PLUS MINUS TIMES DIV LESSER GREATER MOD AND OR EQUAL

%start expr
%%

expr:
  INT         { Econst($1) }
| VAR         { Evar($1) }
| TRUE        { Ebool(true) }
| FALSE       { Ebool(false) }
| expr PLUS expr     { Ebinop(Add, $1, $3) }
| expr MINUS expr    { Ebinop(Sub, $1, $3) }
| expr TIMES expr    { Ebinop(Mul, $1, $3) }
| expr DIV expr      { Ebinop(Div, $1, $3) }
| expr LESSER expr   { Ebinop(Lesser, $1, $3) }
| expr GREATER expr  { Ebinop(Greater, $1, $3) }
| expr MOD expr      { Ebinop(Mod, $1, $3) }
| expr AND expr      { Ebinop(And, $1, $3) }
| expr OR expr       { Ebinop(Or, $1, $3) }
| expr EQUAL expr    { Ebinop(Equal, $1, $3) }
;

(* ast_printer.ml *)
open Ast

let rec string_of_expr = function
  | Econst n -> string_of_int n
  | Evar v -> v
  | Ebool b -> string_of_bool b
  | Ebinop (op, e1, e2) ->
    let op_str = match op with
      | Add -> "+"
      | Sub -> "-"
      | Mul -> "*"
      | Div -> "/"
      | Lesser -> "<"
      | Greater -> ">"
      | Mod -> "%"
      | And -> "&&"
      | Or -> "||"
      | Equal -> "=="
    in
    "(" ^ string_of_expr e1 ^ " " ^ op_str ^ " " ^ string_of_expr e2 ^ ")"

let print_expr expr =
  print_endline (string_of_expr expr)

(* main.ml *)
open Parser
open Lexer
open Ast_printer

let () =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Lexer.token lexbuf in
  print_expr expr   
*)