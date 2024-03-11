(* DEBUG FILE!!! *)


open Ast

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