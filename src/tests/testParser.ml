open OUnit2
open Parser

let tests =
  [
    "parse user" >:: (fun _ ->
        assert_equal "jasper" (get_from_user(parse "yeet|1234|jasper|hello")));
    "parse message" >:: (fun _ ->
        assert_equal "hello" (make_message (get_message(parse "yeet|1234|jasper|hello"))));
    "parse invalid string" >:: (fun _ ->
        assert_raises
          (Failure "ill-formatted string")
          (fun () -> parse "hello"));
    "make string list" >:: (fun _ ->
        assert_equal ["\027[0m"; "h"; "e"; "l"; "l"; "o"; "\027[0m"] (output_list (parse "yeet|1234|jasper|hello")));
    "make string list with formatting" >:: (fun _ ->
        assert_equal ["\027[31m"; "h"; "e"; "l"; "l"; "o"; "\027[0m"] (output_list (parse "yeet|1234|jasper||rhello")));
    "make string list, multiple formats" >:: (fun _ ->
        assert_equal ["\027[31m"; "h"; "e"; "\027[32m"; "l"; "l"; "o"; "\027[0m"] (output_list (parse "yeet|1234|jasper||rhe|gllo")))
  ]

let _ = run_test_tt_main ("test suite for Parser" >::: tests)
