open Ast
let check_int x =
  match x with
  | Some(ConstInt _) -> Printf.printf "This is an int\n"
  | Some _ -> Printf.printf "This is not an int (non-integer expression)\n"
  | None -> Printf.printf "Not initialized \n"

let check_fields fields =
  List.iter (fun field ->
    match field with
    | FieldDef(typespec, _, expr) ->
      match typespec with
        | Int -> check_int expr
        | Float -> Printf.printf "float \n"
        | Bool -> Printf.printf "bool \n"
        | Generic ""-> Printf.printf "Generic \n"
        | _ -> Printf.printf "fail \n"
  )fields


(*
let type_check fts mts =
  List.iter (fun field_list ->
    match field_list with
    | Int -> Printf.printf "Int \n"
    | Float -> Printf.printf "float \n"
    | Bool -> Printf.printf "bool \n"
    | Generic ""-> Printf.printf "Generic \n"
    | _ -> Printf.printf "fail \n"
  )fts;

  List.iter (fun method_list ->
    match method_list with
    | Int -> Printf.printf "Int \n"
    | Float -> Printf.printf "float \n"
    | Bool -> Printf.printf "bool \n"
    | Generic ""-> Printf.printf "Generic \n"
    | _ -> Printf.printf "fail \n"
  )mts
*)
let check_type (game_class, classes) = 
  match game_class with
  | ClassStmt _ -> Printf.printf "Game\n"; 

  List.iter (fun clas ->
    match clas with 
    | ClassStmt (_, (fl, _,_,_)) -> check_fields fl
  ) classes
  
  
  
  