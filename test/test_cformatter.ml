open OUnit2
open Cformatter

(* Assuming Ast and Ctree modules are defined somewhere *)

let test_convert_ast_binop_to_ctree_binop _ =
  let test_cases = [
    (Ast.BinopAdd, Ctree.BinopAdd);
    (Ast.BinopSub, Ctree.BinopSub);
    (Ast.BinopMul, Ctree.BinopMul);
    (Ast.BinopDiv, Ctree.BinopDiv);
    (Ast.BinopMod, Ctree.BinopMod);
    (Ast.BinopAnd, Ctree.BinopAnd);
    (Ast.BinopOr, Ctree.BinopOr);
    (Ast.BinopLessThan, Ctree.BinopLessThan);
    (Ast.BinopGreaterThan, Ctree.BinopGreaterThan);
    (Ast.BinopLessThanEq, Ctree.BinopLessThanEq);
    (Ast.BinopGreaterThanEq, Ctree.BinopGreaterThanEq);
    (Ast.BinopEq, Ctree.BinopEq);
    (Ast.BinopNotEq, Ctree.BinopNotEq)
  ] in
  List.iter (fun (input, expected) ->
    assert_equal expected (convert_ast_binop_to_ctree_binop input)
  ) test_cases

let test_convert_ast_unop_to_ctree_unop _ =
  let test_cases = [
    (Ast.UnopNot, Ctree.UnopNot);
    (Ast.UnopNeg, Ctree.UnopNeg)
  ] in
  List.iter (fun (input, expected) ->
    assert_equal expected (convert_ast_unop_to_ctree_unop input)
  ) test_cases

let test_convert_ast_expr_to_ctree_expr _ =
  let test_cases = [
    (Ast.ConstInt 42, Ctree.ConstInt 42);
    (Ast.ConstFloat 3.14, Ctree.ConstInt (int_of_float (3.14 *. 4096.0)));
    (Ast.Var "x", Ctree.Var "x");
    (Ast.Bool true, Ctree.Bool true);
    (Ast.UnaryOp (Ast.UnopNeg, Ast.ConstInt 5), Ctree.UnaryOp (Ctree.UnopNeg, Ctree.ConstInt 5));
    (Ast.BinaryOp (Ast.BinopAdd, Ast.ConstInt 3, Ast.ConstInt 4), Ctree.BinaryOp (Ctree.BinopAdd, Ctree.ConstInt 3, Ctree.ConstInt 4))
  ] in
  List.iter (fun (input, expected) ->
    assert_equal expected (convert_ast_expr_to_ctree_expr input)
  ) test_cases

let test_convert_ast_typespec_to_ctree_typespec _ =
  let test_cases = [
    (Ast.Int, Ctree.Int);
    (Ast.Float, Ctree.Int);
    (Ast.Bool, Ctree.Bool);
    (Ast.Void, Ctree.Void);
    (Ast.Generic "T", Ctree.Generic "T")
  ] in
  List.iter (fun (input, expected) ->
    assert_equal expected (convert_ast_typespec_to_ctree_typespec input)
  ) test_cases

let test_convert_ast_stmt_to_ctree_stmt _ =
  let test_cases = [
    (Ast.VarDefI (Ast.Int, "x", Ast.ConstInt 10), Ctree.VarDefI (Ctree.Int, "x", Ctree.ConstInt 10));
    (Ast.VarDefU (Ast.Float, "y"), Ctree.VarDefU (Ctree.Int, "y"));
    (Ast.Assign ("z", Ast.ConstInt 20), Ctree.Assign ("z", Ctree.ConstInt 20))
  ] in
  List.iter (fun (input, expected) ->
    assert_equal expected (convert_ast_stmt_to_ctree_stmt input)
  ) test_cases

let suite =
  "formatter_tests" >:::
  [
    "test_convert_ast_binop_to_ctree_binop" >:: test_convert_ast_binop_to_ctree_binop;
    "test_convert_ast_unop_to_ctree_unop" >:: test_convert_ast_unop_to_ctree_unop;
    "test_convert_ast_expr_to_ctree_expr" >:: test_convert_ast_expr_to_ctree_expr;
    "test_convert_ast_typespec_to_ctree_typespec" >:: test_convert_ast_typespec_to_ctree_typespec;
    "test_convert_ast_stmt_to_ctree_stmt" >:: test_convert_ast_stmt_to_ctree_stmt;
  ]

let () =
  run_test_tt_main suite
