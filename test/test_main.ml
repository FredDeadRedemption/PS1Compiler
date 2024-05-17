open OUnit2

let test_addition _ =
  let result = 1 + 1 in
  assert_equal 2 result

let suite =
  "Simple Test Suite" >::: [
    "test_addition" >:: test_addition;
  ]

let () =
  run_test_tt_main suite