open Ast
let check_int x =
  match x with
  | Some(ConstInt _) -> Printf.printf "This is an int\n"
  | Some _ -> failwith("This is not an int\n")
  | None -> Printf.printf "Not initialized \n"
let check_float x =
  match x with
  | Some(ConstFloat _) -> Printf.printf "This is a float\n"
  | Some _ -> failwith("This is not a float\n")
  | None -> Printf.printf "Not initialized \n"

let check_bool x =
  match x with
  | Some(Bool) -> Printf.printf "This is a bool\n"
  | Some _ -> failwith("This is not a bool\n")
  | None -> Printf.printf "Not initialized \n"

let check_void x =
  match x with
    | Some(Void) -> Printf.printf "This is a void\n"
    | Some _ -> failwith("This is not a void\n")
    | None -> Printf.printf("Not initialized \n")

let check_fields fields =
  List.iter (fun field ->
    match field with
    | FieldDef(typespec, _, expr) ->
      match typespec with
        | Int -> check_int (expr)
        | Float -> check_float (expr)
        (*| Bool -> check_bool (Some(expr))*)
        | Generic ""-> Printf.printf "Generic \n"
        | _ -> Printf.printf "fail \n"
  ) fields

let check_formal_int typespec =
  match typespec with
  | Int -> Printf.printf "This is a formal int\n"
  | _ -> failwith "this is not an int"

let check_method_int typespec =
  match typespec with
  | Int -> Printf.printf "This is a method int\n"
  | _ -> failwith "this is not an int"

  

let check_methods methods =
  List.iter (fun meth ->
      match meth with
      | MethodDef(typespec, _, formal, _) ->
        match typespec with 
        | Int -> check_method_int typespec 
        | _ -> ();
        List.iter (fun (formal_typespec, _) ->
          match formal_typespec with
          | Int -> check_formal_int formal_typespec
          | _ -> ()
      ) formal;
  ) methods

  
  
let check_type (game_class, classes) = 
  match game_class with
  | ClassStmt (_, (fl,_,_,_))-> check_fields fl; 

  List.iter (fun clas ->
    match clas with 
    | ClassStmt (_, (fl, _,_,_)) -> check_fields fl
  ) classes;

  List.iter (fun clas ->
    match clas with 
    | ClassStmt (_, (_, _,_,ml)) -> check_methods ml
  ) classes
  
  
  
  