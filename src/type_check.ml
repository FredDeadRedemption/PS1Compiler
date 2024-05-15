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
  | Bool -> Printf.printf "This is a bool\n"
  | _ -> failwith("This is not a bool\n")
*)
let check_fields fields =
  List.iter (fun field ->
    match field with
    | FieldDefI(typespec, _, expr) ->
      (match typespec with
        | Int -> check_int expr
        | Float -> check_float expr
        (*| Bool -> check_bool expr*)
        | Generic ""-> Printf.printf "Generic\n"
        | _ -> failwith "Wrong field type \n")
    | FieldDefU (typ, _) -> 
      (match typ with
        | Int -> Printf.printf"Uninitialized int\n"
        | Float -> Printf.printf"Uninitialized float\n"
        | Bool -> Printf.printf"Uninitialized bool\n"
        | Generic "" -> Printf.printf"Uninitialized Generic\n"
        | _ -> failwith "Wrong field type\n"
  )) fields

let check_methods methods =
  List.iter (fun metho ->
      match metho with
      | MethodDef(typespec, _, formal, _) ->
        (match typespec with 
        | Int -> Printf.printf("Int\n")
        | Float -> Printf.printf("float\n")
        | Bool -> Printf.printf("Bool\n")
        | Void -> Printf.printf("Void\n")
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
  
  
  
  