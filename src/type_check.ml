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

(*let check_methods methods =
  List.iter (fun meth ->
    match meth with
    | MethodDef(typespec, _, formals, _) ->
      match typespec with
        | Int -> check_int (formals)
        | Float -> check_float (formals)
        (*| Bool -> check_bool (Some(expr))*)
        | Generic ""-> Printf.printf "Generic \n"
        | _ -> Printf.printf "fail \n"
  ) methods*)

let check_type (game_class, classes) = 
  match game_class with
  | ClassStmt (_, (fl,_,_,_))-> check_fields fl; 

  List.iter (fun clas ->
    match clas with 
    | ClassStmt (_, (fl, _,_,_)) -> check_fields fl
    (*| ClassStmt (_, (_,_,_,ml)) -> check_methods ml*)
  ) classes
  
  
  
  