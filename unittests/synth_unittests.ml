open Core
open OUnit2
open Util
open My_tree

let test_from_tree_sorts_children _ =
  let module U = UnorderedTreeOf(IntModule) in
  let module N = NormalizedTreeOf(IntModule) in
  let input =
    NonemptyTree (Node (1, [Node (2, []); Node (1, [])]))
  in
  match N.from_tree input with
  | EmptyNTree -> assert_failure "expected NonemptyNTree"
  | NonemptyNTree (NNode (root_label, children, _)) ->
      assert_equal 1 root_label;
      let child_labels = List.map children ~f:(fun (NNode (l, _, _)) -> l) in
      assert_equal [1; 2] child_labels

let normalize_from_tree_suite = "Test NormalizedPTST from_tree" >:::
  [
    "test_from_tree_sorts_children" >:: test_from_tree_sorts_children;
  ]

let _ = run_test_tt_main normalize_from_tree_suite
