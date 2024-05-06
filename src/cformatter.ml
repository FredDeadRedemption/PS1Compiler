open Ast

let the_great_divider ast = 
  match ast with
  | Main b -> 
    List.map (fun stmt ->
      match stmt with
      | ClassStmt (id, body) ->
        match body with
        | FieldDef (ts, id, e) ->
             
        
        | MethodDef (ts, id, args, body) ->
          BreakStmt
        | StartStmt _ -> 
          BreakStmt
        | _ -> 
          (* Generate some formatted tree for function definitions *)
          FuncDef (ts, id, args, body)
      | _ ->
          (* Generate some formatted tree for other statements *)
          BreakStmt
    ) b

let format_to_c ast = 
  let formatted_tree = the_great_divider ast in
  formatted_tree