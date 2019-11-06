open OUnit2
open Parser

let tests =
  [
    "parse user" >:: (fun _ -> 
        assert_equal "user1000" (parse "1000|hello").user);
    "parse message" >:: (fun _ -> 
        assert_equal "hello" (parse "1000|hello").message);
    "parse invalid string" >:: (fun _ -> 
        assert_raises 
          (Failure "ill-formatted string") 
          (fun () -> parse "hello"))
  ]

let _ = run_test_tt_main ("test suite for Parser" >::: tests)
