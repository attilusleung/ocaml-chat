(* The testing suite. *)

(* Our testing was mostly through the chat interface itself. Most of our 
   functionality works through client and server interaction which cannot be 
   tested in this suite. What we tested in this suite were the two parsing tools
   used in our system. 

   We tested Parser and Protocol using black-box testing, developing
   test cases based on each function that we were trying to implement.

   For the parser, we tested the two-directional parsing of messages. We made 
   sure that plaintext could be parsed into Parser types, and that Parser
   types could be converted back into plaintext. We also made sure that text
   formatting instructions were kept throughout the parsing processes.

   For the protocol, we made sure that parsing the protocol messages would
   give us the correct variant. Both directions were tested for protocol
   as well, so that protocol could be decoded as well as encoded. 

   We believe that this suite demonstrates correctness of our transmission
   of information between client and server. Functionality for both directions
   were tested thoroughly. Anything beyond the conversion of information would
   have been tested for manually through our interface and logging. *)

open OUnit2
open Parser
open Protocol

let s = "hmm|1575869243.|yeet||rhi|gbye"

let parser_tests =
  [
    "parse from_user" >:: (fun _ ->
        assert_equal "yeet" (s |> parse |> get_from_user));
    "parse to_user" >:: (fun _ ->
        assert_equal "hmm" (s |> parse |> get_to_user));
    "parse message" >:: (fun _ ->
        assert_equal "|rhi|gbye" (s |> parse |> get_message));
    "parse invalid plaintext" >:: (fun _ ->
        assert_raises
          (Failure "ill-formatted string") (fun () -> parse "this is invalid"));
    "make string list" >:: (fun _ ->
        assert_equal 
          ["["; "0"; "0"; ":"; "2"; "7"; "]"; " "; "y"; "e"; "e"; "t"; ":"; 
           " \027[0m"; "\027[31mh"; "i\027[0m"; "\027[32mb"; "y"; "e\027[0m"] 
          (s |> parse |> output_list));
    "make" >:: (fun _ ->
        assert_equal (parse s) (make "hmm" 1575869243. "yeet" "|rhi|gbye"));
    "pack" >:: (fun _ ->
        assert_equal s (pack "hmm" 1575869243. "yeet" "|rhi|gbye"));
    "pack_t" >:: (fun _ -> assert_equal s (s |> parse |> pack_t));
    "format t" >:: (fun _ ->
        assert_equal "[00:27] yeet: " (s |> parse |> format))
  ]

let protocol_tests = 
  let m = "Mhmm|1575869243.|yeet||rhi|gbye" in
  let l = "Lyeet|1234" in
  let c = "Cyeet" in
  let a = "SAyeet" in
  let r = "SRhmm" in
  let ar = "SAyeet|Rhmm" in
  let f = "Ffailure" in
  [
    "strip head" >:: (fun _ ->
        assert_equal "hmm|1575869243.|yeet||rhi|gbye" (strip_head m));
    "decode message" >:: (fun _ ->
        assert_equal (Message (parse s)) (decode m));
    "decode login" >:: (fun _ ->
        assert_equal (Login ("yeet", "1234")) (decode l));
    "decode malformed login" >:: (fun _ ->
        assert_equal Malformed (decode "hello"));
    "decode confirm" >:: (fun _ ->
        assert_equal (Confirm "yeet") (decode c));
    "decode status accept" >:: (fun _ ->
        assert_equal (Status (["yeet"], [])) (decode a));
    "decode status reject" >:: (fun _ ->
        assert_equal (Status ([], ["hmm"])) (decode r));
    "decode status accept and reject" >:: (fun _ ->
        assert_equal (Status (["yeet"], ["hmm"])) (decode ar));
    "decode malformed" >:: (fun _ ->
        assert_equal Malformed (decode "SAyeet|foo"));
    "decode fail" >:: (fun _ ->
        assert_equal (Fail "failure") (decode f));
    "encode message" >:: (fun _ -> 
        assert_equal m (encode_msg "hmm" 1575869243. "yeet" "|rhi|gbye"));
    "encode parsed message" >:: (fun _ ->
        assert_equal m (s |> parse |> encode_parsed_msg));
    "encode login" >:: (fun _ ->
        assert_equal l (encode_login "yeet" "1234"));
    "encode confirm" >:: (fun _ ->
        assert_equal c (encode_confirm "yeet"));
    "encode status accept" >:: (fun _ ->
        assert_equal a (encode_status ["yeet"] []));
    "encode status reject" >:: (fun _ ->
        assert_equal r (encode_status [] ["hmm"]));
    "encode status accept reject" >:: (fun _ ->
        assert_equal ar (encode_status ["yeet"] ["hmm"]));
  ]

let _ = 
  let tests = parser_tests @ protocol_tests in
  run_test_tt_main ("test suite for Parser" >::: tests)
