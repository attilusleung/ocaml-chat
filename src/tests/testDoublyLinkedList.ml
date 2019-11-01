open OUnit2
open DoublyLinkedList

let tests =
  [
  ]

let suite =
  "test suite for DoublyLinkedList"
  >::: List.flatten tests

let _ = run_test_tt_main suite
