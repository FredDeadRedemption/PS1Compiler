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

(* Test cases *)
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

let test_parser_2 _ =
  let input = "
  class Game {
      start() {
          
      }

      update() {

      }
  }
  
  class Car {
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
      [ClassStmt (
        "Car",
        (
          [], (* No fields *)
          StartDef [], (* Empty start block *)
          UpdateDef [], (* Empty update block *)
          [] (* No methods *)
        )
      )] (* No additional classes *)
    )
  in
  match parse_string input with
  | Some result -> assert_equal expected result
  | None -> assert_failure "Parsing failed"

let test_parser_3 _ =
  let input = "
  class Game {
    int x;
    int xi = 1;
    float y;
    float yi = 1.0;
      start() {
          x = 2;
          xi = 2;
          y = 2.0;
          yi = 2.0;
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
      [ 
        FieldDefU (Int, "x"); 
        FieldDefI (Int, "xi", ConstInt 1); 
        FieldDefU (Float, "y"); 
        FieldDefI (Float, "yi", ConstFloat 1.0) 
      ],
      StartDef [ 
        Assign ("x", ConstInt 2); 
        Assign ("xi", ConstInt 2); 
        Assign ("y", ConstFloat 2.0); 
        Assign ("yi", ConstFloat 2.0) 
      ],
      UpdateDef [],
      []
    )
  ), []
    )
  in
  match parse_string input with
  | Some result -> assert_equal expected result
  | None -> assert_failure "Parsing failed"

let test_parser_variables _ =
  let input = "
  class Game {
    int x;
    float y;
      start() {
          x = 2;
          y = 2.0;
      }

      update() {
          x = 5;
      }
  }
  " in
  let expected =
    (
      ClassStmt (
        "Game",
        (
          [ 
            FieldDefU (Int, "x"); 
            FieldDefU (Float, "y"); 
          ],
          StartDef [ 
            Assign ("x", ConstInt 2); 
            Assign ("y", ConstFloat 2.0) 
          ],
          UpdateDef [ 
            Assign ("x", ConstInt 5) 
          ],
          []
        )
      ),
      []
    )
  in
  match parse_string input with
  | Some result -> assert_equal expected result
  | None -> assert_failure "Parsing failed"

let test_parser_if_else _ =
  let input = "
  class Game {
      start() {
          if (x < 10) {
              x = 5;
          } else {
              x = 15;
          }
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
          [],
          StartDef [ 
            IfStmt (
              BinaryOp (BinopLessThan, Var "x", ConstInt 10),
              [Assign ("x", ConstInt 5)]
            );
            ElseStmt [Assign ("x", ConstInt 15)]
          ],
          UpdateDef [],
          []
        )
      ),
      []
    )
  in
  match parse_string input with
  | Some result -> assert_equal expected result
  | None -> assert_failure "Parsing failed"

  let test_parser_methods _ =
    let input = "
    class Game {
  
        start() {

        }
  
        update() {
            
        }
  
        int y(int x) {
            return 42;
        } 
    }
    " in
    let expected =
      (
        ClassStmt (
          "Game",
          (
            [

            ],
            StartDef [

            ],
            UpdateDef [],
            [
              MethodDef (Int, "y", [(Int, "x")], [ReturnStmt (ConstInt 42)]);
            ]
          )
        ),
        []
      )
    in
    match parse_string input with
    | Some result -> assert_equal expected result
    | None -> assert_failure "Parsing failed"
  

let test_parser_inheritance _ =
  let input = "
  class Player : GameObject {
      start() {
          
      }

      update() {

      }
  }
  " in
  let expected =
    (
      ClassInherStmt (
        "Player",
        "GameObject",
        (
          [],
          StartDef [],
          UpdateDef [],
          []
        )
      ),
      []
    )
  in
  match parse_string input with
  | Some result -> assert_equal expected result
  | None -> assert_failure "Parsing failed"

(* Suite combining the tests *)
let suite =
  "Parser Test Suite" >::: [
    "test_parser" >:: test_parser;
    "test_parser_2" >:: test_parser_2;
    "test_parser_3" >:: test_parser_3;
    "test_parser_variables" >:: test_parser_variables;
    "test_parser_if_else" >:: test_parser_if_else;
    "test_parser_methods" >:: test_parser_methods;
    "test_parser_inheritance" >:: test_parser_inheritance;
    (* Add other test cases here *)
  ]

let () =
  run_test_tt_main suite
