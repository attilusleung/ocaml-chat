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

let s1 = "hmm|1575869243.|yeet||rhi|gbye"
let s2 = "yeet|1575869243.|hmm||||"
let s3 = "...|999999999.|!@#$||b..|y.."

let parser_tests =
  [
    "parse from_user 1" >:: (fun _ ->
        assert_equal "yeet" (s1 |> parse |> get_from_user));
    "parse from_user 2" >:: (fun _ ->
        assert_equal "hmm" (s2 |> parse |> get_from_user));
    "parse from_user 3" >:: (fun _ ->
        assert_equal "!@#$" (s3 |> parse |> get_from_user));
    "parse to_user 1" >:: (fun _ ->
        assert_equal "hmm" (s1 |> parse |> get_to_user));
    "parse to_user 2" >:: (fun _ ->
        assert_equal "yeet" (s2 |> parse |> get_to_user));
    "parse to_user 3" >:: (fun _ ->
        assert_equal "..." (s3 |> parse |> get_to_user));
    "parse message 1" >:: (fun _ ->
        assert_equal "|rhi|gbye" (s1 |> parse |> get_message));
    "parse message 2" >:: (fun _ ->
        assert_equal "|||" (s2 |> parse |> get_message));
    "parse message 3" >:: (fun _ ->
        assert_equal "|b..|y.." (s3 |> parse |> get_message));
    "parse invalid plaintext" >:: (fun _ ->
        assert_raises
          (Failure "ill-formatted string") (fun () -> parse "this is invalid"));
    "make string list 1" >:: (fun _ ->
        assert_equal 
          ["["; "0"; "0"; ":"; "2"; "7"; "]"; " "; "y"; "e"; "e"; "t"; ":"; 
           " \027[0m"; "\027[31mh"; "i\027[0m"; "\027[32mb"; "y"; "e\027[0m"] 
          (s1 |> parse |> output_list));
    "make string list 2" >:: (fun _ ->
        assert_equal 
          ["["; "0"; "0"; ":"; "2"; "7"; "]"; " "; "h"; "m"; "m"; ":"; 
           " \027[0m"; "\027[0m|\027[0m"; "\027[0m|\027[0m"; "\027[0m|\027[0m"] 
          (s2 |> parse |> output_list));
    "make string list 3" >:: (fun _ ->
        assert_equal 
          ["["; "2"; "1"; ":"; "4"; "6"; "]"; " "; "!"; "@"; "#"; "$"; ":"; 
           " \027[0m"; "\027[1m."; ".\027[0m"; "\027[33m."; ".\027[0m"]
          (s3 |> parse |> output_list));
    "make 1" >:: (fun _ ->
        assert_equal (parse s1) (make "hmm" 1575869243. "yeet" "|rhi|gbye"));
    "make 2" >:: (fun _ ->
        assert_equal (parse s2) (make "yeet" 1575869243. "hmm" "|||"));
    "make 3" >:: (fun _ ->
        assert_equal (parse s3) (make "..." 999999999. "!@#$" "|b..|y.."));
    "pack 1" >:: (fun _ ->
        assert_equal s1 (pack "hmm" 1575869243. "yeet" "|rhi|gbye"));
    "pack 2" >:: (fun _ ->
        assert_equal s2 (pack "yeet" 1575869243. "hmm" "|||"));
    "pack 3" >:: (fun _ ->
        assert_equal s3 (pack "..." 999999999. "!@#$" "|b..|y.."));
    "pack_t 1" >:: (fun _ -> assert_equal s1 (s1 |> parse |> pack_t));
    "pack_t 2" >:: (fun _ -> assert_equal s2 (s2 |> parse |> pack_t));
    "pack_t 3" >:: (fun _ -> assert_equal s3 (s3 |> parse |> pack_t));
    "format 1" >:: (fun _ ->
        assert_equal "[00:27] yeet: " (s1 |> parse |> format));
    "format 2" >:: (fun _ ->
        assert_equal "[00:27] hmm: " (s2 |> parse |> format));
    "format 3" >:: (fun _ ->
        assert_equal "[21:46] !@#$: " (s3 |> parse |> format));
  ]

let protocol_tests = 
  let m = "Mhmm|1575869243.|yeet||rhi|gbye" in
  let m' = "Myeet|1575869243.|hmm||||" in
  let m'' = "M...|999999999.|!@#$||b..|y.." in
  let l = "Lyeet|1234" in
  let l' = "Lhmm|1" in
  let l'' = "L!@#$|||||" in
  let c = "Cyeet" in
  let a = "SAyeet" in
  let r = "SRhmm" in
  let ar = "SAyeet|Rhmm" in
  let f = "Ffailure" in
  [
    "decode message 1" >:: (fun _ ->
        assert_equal (Message (parse s1)) (decode m));
    "decode message 2" >:: (fun _ ->
        assert_equal (Message (parse s2)) (decode m'));
    "decode message 3" >:: (fun _ ->
        assert_equal (Message (parse s3)) (decode m''));
    "decode login 1" >:: (fun _ ->
        assert_equal (Login ("yeet", "1234")) (decode l));
    "decode login 2" >:: (fun _ ->
        assert_equal (Login ("hmm", "1")) (decode l'));
    "decode login 3" >:: (fun _ ->
        assert_equal (Login ("!@#$", "||||")) (decode l''));
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
    "encode message 1" >:: (fun _ -> 
        assert_equal m (encode_msg "hmm" 1575869243. "yeet" "|rhi|gbye"));
    "encode message 2" >:: (fun _ -> 
        assert_equal m' (encode_msg "yeet" 1575869243. "hmm" "|||"));
    "encode message 3" >:: (fun _ -> 
        assert_equal m'' (encode_msg "..." 999999999. "!@#$" "|b..|y.."));
    "encode parsed message 1" >:: (fun _ ->
        assert_equal m (s1 |> parse |> encode_parsed_msg));
    "encode parsed message 2" >:: (fun _ ->
        assert_equal m' (s2 |> parse |> encode_parsed_msg));
    "encode parsed message 3" >:: (fun _ ->
        assert_equal m'' (s3 |> parse |> encode_parsed_msg));
    "encode login 1" >:: (fun _ ->
        assert_equal l (encode_login "yeet" "1234"));
    "encode login 2" >:: (fun _ ->
        assert_equal l' (encode_login "hmm" "1"));
    "encode login 3" >:: (fun _ ->
        assert_equal l'' (encode_login "!@#$" "||||"));
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
