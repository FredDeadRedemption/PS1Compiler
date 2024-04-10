open Ast

let rec print_expr expr =
  match expr with
  | Econst c -> Printf.printf "Econst(%d)" c
  | Efloat f -> Printf.printf "Efloat(%f)" f
  | Evar v -> Printf.printf "Evar(%s)" v
  | Ebool b -> Printf.printf "Ebool(%s)" (string_of_bool b);
  | Efuncall (n, a) -> Printf.printf "Efuncall(name: %s" n;
    List.iter (fun arg -> Printf.printf " arg: "; print_expr arg) a; 
  | Eunop (o, e) ->
    let opii = string_of_unop o in
    Printf.printf "Eunop(%s, " opii;
    print_expr e;
    Printf.printf ")"  
  | Ebinop (op, e1, e2) -> 
    let opii = string_of_binop op in
    Printf.printf "Ebinop(%s, " opii;
    print_expr e1;
    Printf.printf ", ";
    print_expr e2;
    Printf.printf ")"

let print_typespec ts =
  let str = string_of_typespec ts in
  Printf.printf "%s" str

let rec print_stmt stmt =
  match stmt with
  | Sblock block ->
      Printf.printf "Sblock [\n";
      List.iter (fun stmt ->  print_stmt stmt; Printf.printf ";\n") block; (* SEMI COLON VED ALLE BLOCKS*)
      Printf.printf "\t]"
  | Sprint stmt ->
      Printf.printf "\tSprint: \"";
      print_expr stmt;
      Printf.printf "\""
  | Sstart stmts ->
      Printf.printf "\tSstart [\n";
      List.iter (fun stmt -> Printf.printf "\t"; print_stmt stmt; Printf.printf ";\n") stmts;
      Printf.printf "\t]"
  | Sif (e, b) ->
      Printf.printf "\tSif [\n";
      Printf.printf "expr: "; print_expr e;
      Printf.printf "then: "; 
      List.iter (fun stmt -> print_stmt stmt) b;
  | Selseif (e, b) ->
      Printf.printf "\tSelseif [\n";
      Printf.printf "expr: "; print_expr e;
      Printf.printf "then: "; 
      List.iter (fun stmt -> print_stmt stmt) b;
  | Selse (b) ->
    Printf.printf "\tSelse [\n";
    Printf.printf "then: "; 
    List.iter (fun stmt -> print_stmt stmt) b;
      
  | Sexpr stmt ->
    Printf.printf "\tSexpr: ";
      print_expr stmt;
      Printf.printf "" 
  | Sassign (id, e) ->
    Printf.printf "\nassign: ";
    Printf.printf "varname: %s" id; 
    Printf.printf "newValue: "; print_expr e;           
  | Svardef (ts, id, v) ->
      Printf.printf "\tvariable: type : "; print_typespec ts;
      Printf.printf ", name : %s, " id;
      Printf.printf "value : "; print_expr v; Printf.printf ""
  | Sreturn v -> 
      Printf.printf "\tReturn Statement: "; print_expr v;
  | Sbreak -> Printf.printf "Break";
  | Sfundef (ts, id, f, b) ->
      Printf.printf "\tfunction: type : "; print_typespec ts;
      Printf.printf ", name : %s, " id;
      (*let formals = map_formals f in*)
      List.iter (fun (ts, id) ->
        Printf.printf "\nargument: %s, Name: %s" (string_of_typespec ts) id
      ) f;
      Printf.printf "\nbody : " ;
      List.iter (fun stmt ->  print_stmt stmt; Printf.printf ";\n") b
  
let print_program program =
  Printf.printf "{\n\tdefs = [";
  (*List.iter (fun def -> Printf.printf "\"%s\"; " (extract_def_name def)) program.defs;*)
  Printf.printf "];\n\tmain = ";
  print_stmt program.main;
  Printf.printf "\n}\n"