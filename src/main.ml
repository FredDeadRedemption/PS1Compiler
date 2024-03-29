open Ast

let rec print_expr expr =
  match expr with
  | Econst c -> Printf.printf "Econst(%d)" c
  | Evar v -> Printf.printf "Evar(%s)" v
  | Ebool b -> Printf.printf "Ebool(%b)" b
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
    Printf.printf "Ebinop(%s)" op_str;
    print_expr e1;
    Printf.printf ", ";
    print_expr e2;
    Printf.printf ")"

let rec print_stmt stmt =
  match stmt with
  | Sif (e, s, else_stmt) ->
      Printf.printf "Sif (";
      print_expr e;
      Printf.printf ", ";
      print_stmt s;
      Printf.printf ", [\n";
      (match else_stmt with
       | Sblock else_stmts -> print_stmt (Sblock else_stmts)
       | _ -> print_stmt else_stmt);
      Printf.printf ")"
  | Sblock stmts ->
      Printf.printf "Sblock [\n";
      List.iter (fun stmt -> print_stmt stmt; Printf.printf ";\n") stmts;
      Printf.printf "]"
  | Sempty -> Printf.printf "Sempty"

let extract_def_name def =
  def.name

  (*
let print_program program =
  Printf.printf "{\n  defs = [";
  List.iter (fun def -> Printf.printf "\"%s\"; " (extract_def_name def)) program.defs;
  Printf.printf "];\n  main = ";
  print_stmt program.main;
  Printf.printf "\n}\n"9
*)
let print_program program =
    let rec print_exprs = function
      | [] -> ()
      | expr :: rest ->
          (* print each expression *)
          print_endline "\nExpression: "; (* print your expression here *)
          print_expr expr;
          print_exprs rest
    in
    print_endline "Program:";
    print_exprs program.exprs


let () =
  let filename = Sys.argv.(1) in
  let filehandle = open_in filename in
  let lexbuf = Lexing.from_channel filehandle in
  let prog = Parser.program Lexer.tokenize lexbuf in
  print_program prog






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