open Core
open OUnit2

let test_abc _ =
  assert_equal
    false
    true

let normalize_from_tree_suite = "Test NormalizedPTST from_tree" >:::
  [
    "test_abc" >:: test_abc;
  ]

let _ = run_test_tt_main normalize_from_tree_suite
