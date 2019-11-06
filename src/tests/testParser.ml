open OUnit2
open Parser

let tests =
  [
    "parse hello" >:: (fun _ -> 
        assert_equal 
          {user = "jasper"; time = "1637"; message = "hello"} 
          (parse "jasper|1637|hello"));
    "parse invalid string" >:: (fun _ -> 
        assert_raises 
          (Failure "ill-formatted string") 
          (fun () -> parse "hello"))
  ]

let _ = run_test_tt_main ("test suite for Parser" >::: tests)
