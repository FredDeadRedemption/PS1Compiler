open Ast
let check_int x =
  match x with
  | ConstInt _ -> Printf.printf "This is an int\n"
  | _ -> failwith("Expected int in expression\n")

let check_float x =
  match x with
  | ConstFloat _ -> Printf.printf "This is a float\n"
  | _ -> failwith("Expected float in expression\n")

(*let check_bool x =
  match x with
  | Bool _ -> Printf.printf "This is a bool\n"
  | _ -> failwith("Expected bool in expression\n")*)

let check_fields fields =
  List.iter (fun field ->
    match field with
    | FieldDefI(typespec, _, expr) ->
      (match typespec with
        | Int -> check_int expr
        | Float -> check_float expr
        (*| Bool -> check_bool expr*)
        | Generic _ -> Printf.printf "Generic\n"
        | _ -> failwith "Wrong field type \n");

    | FieldDefU (typ, _) -> 
      (match typ with
        | Int -> Printf.printf "Uninitialized int\n"
        | Float -> Printf.printf "Uninitialized float\n"
        | Bool -> Printf.printf "Uninitialized bool\n"
        | Generic _ -> Printf.printf "Uninitialized Generic\n"
        | _ -> failwith "Wrong field type\n"
      );
    | FieldClsInit (_,_) -> Printf.printf "New class initialised\n"
  ) fields

let check_method_int list =
  List.iter (fun stmt ->
    match stmt with 
    | ReturnStmt expr ->
      (match expr with 
      | ConstInt _ -> Printf.printf "Method int return\n"
      | _ -> failwith "Return type doesn't match method typespec \n")
    | _ -> ()
  ) list

let check_method_float list =
  List.iter (fun stmt ->
    match stmt with 
    | ReturnStmt expr ->
      (match expr with 
      | ConstFloat _ -> Printf.printf "Method float return\n"
      | _ -> failwith "Return type doesn't match method typespec \n")
    | _ -> ()
  ) list

let check_method_bool list =
  List.iter (fun stmt ->
    match stmt with 
    | ReturnStmt expr ->
      (match expr with 
      | Bool _ -> Printf.printf "Method bool return\n"
      | _ -> failwith "Return type doesn't match method typespec \n")
    | _ -> ()
  ) list

let check_method_void block =
  List.iter (fun stmt ->
    match stmt with 
    | ReturnStmt expr ->
      (match expr with 
      | ConstInt _ -> failwith "Expected no return value for void method, but got int\n"
      | ConstFloat _ -> failwith "Expected no return value for void method, but got float\n"
      | Bool _ -> failwith "Expected no return value for void method, but got bool\n"
      | Var _ -> failwith "Expected no return value for void method, but got variable\n"
      | FuncCall _ -> failwith "Expected no return value for void method, but got function call\n"
      | _ -> Printf.printf "Expected no return value\n")
    | _ -> ()
  ) block

let check_methods methods =
  List.iter (fun metho ->
      match metho with
      | MethodDef(typespec, _, formal, block) ->
        (match typespec with 
        | Int -> check_method_int block
        | Float -> check_method_float block
        | Bool -> check_method_bool block
        | Void -> check_method_void block
        | _ -> failwith "method typespec error\n" );
        
        List.iter (fun (formal_typespec, _) ->
          match formal_typespec with
          | Int -> Printf.printf("int\n")
          | Float -> Printf.printf("float\n")
          | Bool -> Printf.printf("Bool\n")
          | Void -> Printf.printf("Void\n")
          | _ -> failwith "method formal typespec error\n"
        )formal;
      ) methods

let check_type (game_class, classes) = 
  (match game_class with
    | ClassStmt (_, (fl,_,_,_)) -> check_fields fl
    | ClassInherStmt (_, _, (fl, _, _, _)) -> check_fields fl
  );

  (match game_class with
    | ClassStmt (_, (_,_,_,ml)) -> check_methods ml
    | ClassInherStmt (_, _, (_, _, _, ml)) -> check_methods ml
  );
  List.iter (fun clas ->
    match clas with 
    | ClassStmt (_, (fl, _,_,_)) -> check_fields fl
    | ClassInherStmt (_, _, (fl, _, _, _)) -> check_fields fl
  ) classes;

  List.iter (fun clas ->
    match clas with 
    | ClassStmt (_, (_, _,_,ml)) -> check_methods ml
    | ClassInherStmt (_, _, (_, _, _, ml)) -> check_methods ml
  ) classes
  
  
  
  