open OUnit2
open Parser

let tests =
  [
    "parse hello" >:: fun _ -> assert_equal 
        {user = "jasper"; time = "1637"; message = "hello"} 
        (parse "jasper|1637|hello")
  ]

let _ = run_test_tt_main ("test suite for Parser" >::: tests)
