open OUnit2
open Ast

(* Helper function to parse a string *)
let parse_string str =
  let lexbuf = Lexing.from_string str in
  try
    let prog = Parser.program Lexer.tokenize lexbuf in
    Some prog
  with
  | Parser.Error -> None

(* Test case for parser *)
let test_parser _ =
  let input = "
  class Game {
      start() {
          
      }

      update() {

      }
  }
  " in
  let expected =
    (
      ClassStmt (
        "Game",
        (
          [], (* No fields *)
          StartDef [], (* Empty start block *)
          UpdateDef [], (* Empty update block *)
          [] (* No methods *)
        )
      ),
      [] (* No additional classes *)
    )
  in
  match parse_string input with
  | Some result -> assert_equal expected result
  | None -> assert_failure "Parsing failed"

(* Suite combining the tests *)
let suite =
  "Parser Test Suite" >::: [
    "test_parser" >:: test_parser;
    (* Add other test cases here *)
  ]

let () =
  run_test_tt_main suite