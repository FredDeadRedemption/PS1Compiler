open Ast

let check_int x =
  match x with
  | ConstInt _ -> Printf.printf "This is an int\n"
  | _ -> failwith "Expected int in expression\n"

let check_float x =
  match x with
  | ConstFloat _ -> Printf.printf "This is a float\n"
  | _ -> failwith "Expected float in expression\n"

(*let check_bool x =
    match x with
    | Bool _ -> Printf.printf "This is a bool\n"
    | _ -> failwith "Expected bool in expression\n"*)


let check_generic expr =
  match expr with
  | Generic _ -> Printf.printf "Expression is generic\n"
  | _ -> Printf.printf "Expression is not generic\n"

let check_fields fields =
  List.iter (fun field ->
    match field with
    | FieldDefI(typespec, _, expr) ->
      (match typespec with
        | Int -> check_int expr
        | Float -> check_float expr 
        | Bool -> Printf.printf "Bool\n"
        | Generic _ -> Printf.printf "Generic\n"
        | _ -> failwith "Wrong field type\n")
    | FieldDefU (typ, _) -> 
      (match typ with
        | Int -> Printf.printf "Uninitialized int\n"
        | Float -> Printf.printf "Uninitialized float\n"
        | Bool -> Printf.printf "Uninitialized bool\n" 
        | Generic _ -> Printf.printf "Uninitialized Generic\n"
        | _ -> failwith "Wrong field type\n")
    | FieldClsInit (_, _) -> Printf.printf "New class initialized\n"
  ) fields

let check_method_int stmts =
  List.iter (fun stmt ->
    match stmt with 
    | ReturnStmt expr ->
      (match expr with 
      | ConstInt _ -> Printf.printf "Method int return\n"
      | Var str -> 
        (match str with
          | _ -> Printf.printf "%s\n" str
         )
      | _ -> failwith "Return type doesn't match method typespec\n")
    | _ -> ()
  ) stmts

let check_method_float stmts =
  List.iter (fun stmt ->
    match stmt with 
    | ReturnStmt expr ->
      (match expr with 
      | ConstFloat _ -> Printf.printf "Method float return\n"
      | Var str -> 
        (match str with
          | _ -> Printf.printf "%s\n" str
         )
      | _ -> failwith "Return type doesn't match method typespec\n")
    | _ -> ()
  ) stmts

let check_method_bool stmts =
  List.iter (fun stmt ->
    match stmt with 
    | ReturnStmt expr ->
      (match expr with 
      | Bool _ -> Printf.printf "Method bool return\n"
      | Var str -> 
        (match str with
          | _ -> Printf.printf "%s\n" str
         )
      | _ -> failwith "Return type doesn't match method typespec\n")
    | _ -> ()
  ) stmts

let check_method_void block =
  List.iter (fun stmt ->
    match stmt with 
    | ReturnStmt expr ->
      (match expr with 
      | ConstInt _ -> failwith "Expected no return value for void method, but got int\n"
      | ConstFloat _ -> failwith "Expected no return value for void method, but got float\n"
      | Bool _ -> failwith "Expected no return value for void method, but got bool\n"
      | Var _ -> failwith "Expected no return value for void method, but got variable\n"
      | _ -> Printf.printf "Expected no return value\n")
    | _ -> ()
  ) block

let check_methods methods =
  List.iter (fun meth ->
    match meth with
    | MethodDef(typespec, _, formals, block) ->
      (match typespec with 
        | Int -> check_method_int block
        | Float -> check_method_float block
        | Bool -> check_method_bool block
        | Void -> check_method_void block
        | _ -> failwith "Method typespec error\n");
      
      List.iter (fun (formal_typespec, _) ->
        match formal_typespec with
        | Int -> Printf.printf "int\n"
        | Float -> Printf.printf "float\n"
        | Bool -> Printf.printf "Bool\n"
        | Void -> Printf.printf "Void\n"
        | Generic _ -> Printf.printf "Generic String \n"
      ) formals
  ) methods

let check_type (game_class, classes) = 
  let check_class c =
    match c with
    | ClassStmt (_, (fields, _, _, methods)) -> 
      check_fields fields;
      check_methods methods
    | ClassInherStmt (_, _, (fields, _, _, methods)) -> 
      check_fields fields;
      check_methods methods
  in
  check_class game_class;
  List.iter check_class classes
