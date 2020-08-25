(** Testing plan:
    Unit testing for each function as functions are written, gradually building 
    test suite as the project scaled.
    Project is also tested with game play after each significant portion of 
    project is coded. 
    Bisect was used to ensure extensive testing.
    Final beta testing of project was done with external users, through the 
    authors' friends to test user experience and edge cases. *)

open OUnit2
open Scrabble
open Board
open Player
open Dictionary

let user1 = {
  score = 0; 
  letters = ['a';'b';'c';'e';'f';'l';'i']; 
  turn = true; 
  turn_message = "Player 1's Turn"
}

let board = empty;;
board.(0).(1) <- 'f'
let (x, y, s1) = 
  Scrabble.make_word ['a';'s';'t'] (0,1) Horizontal board "" 0 true 0
let (z, m, s2) = 
  Scrabble.make_word ['s';'m'] (5,5) Horizontal board "" 0 true 0
let (a, b, s3) = 
  Scrabble.make_word ['f';'a';'s';'t'] (10,5) Horizontal board "" 0 true 0


module EnglishWords = Set.Make(String)

let set = EnglishWords.empty
let file_list = ["p1.txt"; "p2.txt"; "p3.txt"; "p4.txt"; "p5.txt"]
let set1 = Dictionary.indexed_contents "dictionary" file_list set
let empty = Scrabble.empty
let board = Scrabble.empty
let board2 = Scrabble.empty;;
board2.(7).(7) <- 'h';;
board2.(7).(8) <- 'e';;
board2.(7).(9) <- 'l';;
board2.(7).(10) <- 'p';;

let scrabble_tests = [ 
  "make_word test 1">:: (fun _ -> assert_equal "fast" (x));
  "make_word test 2">:: (fun _ -> assert_equal "sm" (z));
  "make_word test 3">:: (fun _ -> assert_equal "fast" (a));

  "check dictionary 1">:: (fun _ -> assert_equal false 
                              (Scrabble.check_dictionary set1 "asbdbfs"));
  "check dictionary 2">:: (fun _ -> assert_equal true 
                              (Scrabble.check_dictionary set1 "make"));
  "check dictionary 3">:: (fun _ -> assert_equal true 
                              (Scrabble.check_dictionary set1 "smile"));
  "check dictionary 4">:: (fun _ -> assert_equal false 
                              (Scrabble.check_dictionary set1 "aabbcc"));
  "check dictionary 4">:: (fun _-> assert_equal true 
                              (Scrabble.check_dictionary set1 "a"));

  (* Testing string_to_char_list *)
  "convert valid one-letter-string list to char list" >:: (fun _ ->
      assert_equal ['a';'b'] (Scrabble.string_to_char_list user1 [] ["a";"b"]));

  "convert valid multiple-letters-string to one-letter char list" >:: (fun _ ->
      assert_equal ['a';'c'] (Scrabble.string_to_char_list user1 [] 
                                ["ape";"cy"]));
  "convert empty list">:: (fun _-> 
      assert_equal [] (Scrabble.string_to_char_list user1 [] []));
  "is_blank 1">:: (fun _ -> assert_equal true (Board.is_blank board (8,8)));
  "is_blank 2">:: (fun _ -> assert_equal false (Board.is_blank board2 (7,8)));

  "convert valid 4 multiple-letters-string to one-letter char list" >:: 
  (fun _ -> assert_equal ['a';'b';'c';'f'] 
      (Scrabble.string_to_char_list user1 [] ["ael";"bel";"cel";"fel"]));

  "raise exception for character not available to player" >:: (fun _ ->
      assert_raises (InvalidChars) 
        (fun () -> Scrabble.string_to_char_list user1 [] ["c";"d"]));

  (* Test new_start_idx *)
  "almost empty board">:: 
  (fun _-> assert_equal (8,8) 
      (Scrabble.new_start_idx (8,8) Horizontal board2));

  "new_start_idx2">:: 
  (fun _-> assert_equal (8,10) 
      (Scrabble.new_start_idx (8,10) Horizontal board2));

  (* Test check_placement *)
  "valid placement of characters">:: (fun _-> 
      assert_equal ("and", 0, 0) (Scrabble.check_placement empty_matrix 
                                    Horizontal (8,8) ['a';'n';'d']));

  "valid placement of non-english word">:: 
  (fun _-> assert_equal ("omg", 0, 0) (Scrabble.check_placement empty Vertical 
                                         (5,5) ['o';'m';'g']));
  "invalid_placement">::
  (fun _-> assert_raises (InvalidPlacement) 
      (fun () -> (Scrabble.check_placement empty Vertical (15,15) 
                    ['f';'a';'i';'l'])));
  "valid placement edge">::
  (fun _-> assert_equal ("edge", 0, 0) 
      (Scrabble.check_placement empty Horizontal (1, 12) ['e';'d';'g';'e']));
  "valid placement edge vertical">::
  (fun _-> assert_equal ("edge", 0, 0) (Scrabble.check_placement empty 
                                          Vertical (12, 1) ['e';'d';'g';'e']));
  "invalid placement edge">::
  (fun _-> assert_raises (InvalidPlacement)
      (fun () -> (Scrabble.check_placement empty Horizontal (1, 13) 
                    ['e';'d';'g';'e'])));
  "invalid placement edge vertical">::
  (fun _-> assert_raises (InvalidPlacement) 
      (fun () -> (Scrabble.check_placement empty Vertical (13, 1) 
                    ['e';'d';'g';'e'])));

  (* Test insert_first *)
  "invalid insert_first horizontal edge">::
  (fun _-> assert_raises (InvalidStart)
      (fun () -> (Scrabble.insert_first board (8,9) Horizontal ['a'])));
  "invalid insert_first horizontal edge 2">::
  (fun _-> assert_raises (InvalidWord)
      (fun () -> (Scrabble.insert_first board (8,7) Horizontal ['a'])));
  "invalid insert_first vertical edge">::
  (fun _-> assert_raises (InvalidStart)
      (fun () -> (Scrabble.insert_first board (9,8) Vertical ['a'])));
  "invalid insert_first vertical edge 2">::
  (fun _-> assert_raises (InvalidStart)
      (fun () -> (Scrabble.insert_first board (3,2) Vertical ['a'])));
  "invalid insert_first word">::
  (fun _-> assert_raises (InvalidWord)
      (fun () -> 
         (Scrabble.insert_first board (8,8) Horizontal ['o';'m';'g'])));


  (* Test insert *)
  "invalid word">::
  (fun _-> assert_raises (InvalidWord)
      (fun () -> (
           let ((f1, f2), s, t) = 
             Scrabble.insert_first (board) (8,8) (Horizontal) 
               (['t';'e';'s';'t']) 
           in Scrabble.insert f1 (8,8) Vertical ['t';'t';'t'])));

  (* Testing insert function *)
  "insert one character at the right spot" >:: (fun _ ->
      assert_equal 's' 
        (let ((f1, _), _, _) = Scrabble.insert board (8,12) Horizontal ['s'] in 
         (f1.(7).(11))));  

  "insert a word in vertical" >:: (fun _ ->
      let word = "apple" in
      let ((f1, _), _, _) = 
        Scrabble.insert board (4,9) Vertical ['a';'p';'p';'l'] in 
      let new_word = begin
        String.make 1 f1.(3).(8) ^ String.make 1 f1.(4).(8) ^
        String.make 1 f1.(5).(8) ^ String.make 1 f1.(6).(8) ^
        String.make 1 f1.(7).(8) 
      end in
      assert_equal word new_word);

  "insert a word in horizontal" >:: (fun _ ->
      let word = "play" in
      let ((board, _), _, _) = 
        Scrabble.insert board (7, 11) Vertical ['a';'p';'l';'e'] in
      let ((f1, _), _, _) = 
        Scrabble.insert board (10, 10) Horizontal ['p';'a';'y'] in 
      let new_word = begin
        String.make 1 f1.(9).(9) ^ String.make 1 f1.(9).(10) 
        ^ String.make 1 f1.(9).(11) ^ String.make 1 f1.(9).(12) 
      end in
      assert_equal word new_word);

  "insert an invalid word into board with intersection" >:: (fun _ ->
      assert_raises (InvalidWord) 
        (fun () -> Scrabble.insert board (5, 9) Horizontal ['z']));

  "insert edge case" >:: (fun _ ->
      let  ((b1, _), _, _) = 
        Scrabble.insert board (11,11) Horizontal ['t'] in
      let new_word = String.make 1 b1.(10).(10) ^ String.make 1 b1.(10).(11) in
      assert_equal "et" new_word);

  "insert edge case - vertical" >:: (fun _ ->
      let  ((b1, _), _, _) = 
        Scrabble.insert board (8,12) Vertical ['e'] in
      let new_word = String.make 1 b1.(7).(11) ^ String.make 1 b1.(8).(11) ^
                     String.make 1 b1.(9).(11) ^ String.make 1 b1.(10).(11) 
      in
      assert_equal "seat" new_word);

  (* Testing parse *)
  "raise exception when command is not useful" >:: (fun _ ->
      assert_raises (InvalidCommand) 
        (fun () -> Scrabble.parse " " board user1));
  "raise exception when no command given" >:: (fun _ ->
      assert_raises (InvalidCommand) 
        (fun () -> Scrabble.parse "" board user1));
  "raise exception when index is invalid" >:: (fun _ ->
      assert_raises (InvalidIndex) 
        (fun () -> Scrabble.parse "16,2 vertical a,b" board user1));
  "raise exception when index is not given" >:: (fun _ ->
      assert_raises (InvalidIndex) 
        (fun () -> Scrabble.parse ",2 vertical a,b" board user1));
  "raise exception when index is not given 2" >:: (fun _ ->
      assert_raises (InvalidIndex) 
        (fun () -> Scrabble.parse ",2,1 vertical a,b" board user1));
  "raise exception when index is invalid row" >:: (fun _ ->
      assert_raises (InvalidIndex) 
        (fun () -> Scrabble.parse "0,2 vertical a,b" board user1));
  "raise exception when index is invalid row horizontal" >:: (fun _ ->
      assert_raises (InvalidIndex) 
        (fun () -> Scrabble.parse "16,2 horizontal a,b" board user1));
  "raise exception when index is invalid col" >:: (fun _ ->
      assert_raises (InvalidIndex) 
        (fun () -> Scrabble.parse "2,16 horizontal a,b" board user1));
  "raise exception when index is invalid row 0 horizontal" >:: (fun _ ->
      assert_raises (InvalidIndex) 
        (fun () -> Scrabble.parse "0,2 horizontal a,b" board user1));
  "raise exception when index is invalid col 0" >:: (fun _ ->
      assert_raises (InvalidIndex) 
        (fun () -> Scrabble.parse "2,0 horizontal a,b" board user1));
  "raise exception when index is invalid col vertical" >:: (fun _ ->
      assert_raises (InvalidIndex) 
        (fun () -> Scrabble.parse "2,16 vertical a,b" board user1));
  "raise exception when index is invalid col 0 vertical" >:: (fun _ ->
      assert_raises (InvalidIndex) 
        (fun () -> Scrabble.parse "2,0 vertical a,b" board user1));
  "raise exception when orientation is invalid" >:: (fun _ ->
      assert_raises (InvalidOrientation) 
        (fun () -> Scrabble.parse "1,2 hjjsk a,b" board user1));
  "parse and return correct board" >:: (fun _ ->
      assert_equal Vertical 
        (let (idx,orientation,list) = 
           (Scrabble.parse "4,1 vertical a,b" board user1) in orientation));
  "raise exception when characters entered is invalid" >:: (fun _ ->
      assert_raises (InvalidChars) 
        (fun () -> Scrabble.parse "4,1 vertical d,g" board user1));
  "raise exception when characters entered is horizontal" >:: (fun _ ->
      assert_raises (InvalidChars) 
        (fun () -> Scrabble.parse "4,1 horizontal d,g" board user1));
  "raise exception when given no characters">:: (fun _->
      assert_raises (InvalidChars)
        (fun () -> Scrabble.parse "3,4 horizontal " board user1));
  "raise exception when given no characters vertical">:: (fun _->
      assert_raises (InvalidChars)
        (fun () -> Scrabble.parse "3,4 vertical " board user1));

  (* Testing score_helper *)
  "score_helper test 1" >:: (fun _ ->
      assert_equal (true, ("empty", None))
        (Scrabble.score_helper (5,7) board 'a'));
  "score_helper test 2" >:: (fun _ ->
      assert_equal (true, ("TW", None))
        (Scrabble.score_helper (0,0) board 'b'));
  "score_helper test 3" >:: (fun _ ->
      assert_equal (true, ("DL", Some 'b'))
        (Scrabble.score_helper (3,0) board 'b'));
]

let player = {
  score = 0; 
  letters = ['a';'b';'c';'d';'e']; 
  turn = true; 
  turn_message = "Player 1 Turn"
}
let board = empty;;

let board = Scrabble.empty;;

let rec print_list = function 
    [] -> print_newline ()
  | h::t -> print_char h ; print_string " " ; print_list t

let rec print_tuple_list = function 
    [] -> print_newline ()
  | (i, _)::t -> print_char i ; print_string " " ; print_tuple_list t

let player1 = {
  score = 0;
  letters = ['a';'b';'g';'e';'y';'r';'p'];
  turn = true;
  turn_message = "Player 1";
}

let player2 = Player.init_state_first 

let list = Player.filter ['g';'a'] (Player.char_list player2)
let player2 = Player.update "" list player2 [] 0

let player3 = Player.init_state_first
let list2 = Player.filter [] (Player.char_list player3)
let player3 = Player.update "" list2 player3 [] 0

let player4 = Player.init_state_first
let list3 = Player.filter ['g'; 'a'; 't'; 'h'; 'g'; 'b'; 'o'] 
    (Player.char_list player4)
let player4 = Player.update "" list2 player4 [] 0

let player5 = Player.init_state_first
let player6 = Player.init_state_second

let player_tests = [
  "remove test 1">:: (
    fun _ -> assert_equal ['a';'b';'g';'e';'y'] 
        (Player.filter ['r';'p'] (Player.char_list player1)));

  "remove test 2">:: (fun _ -> 
      assert_equal [] 
        (Player.filter ['a';'b';'g';'e';'y';'r';'p'] 
           (Player.char_list player1)));

  "remove test 3">:: (fun _ -> 
      assert_equal ['a';'b';'g';'e';'y';'r';'p'] 
        (Player.char_list player1));

  "char_list test 1">:: (fun _ -> 
      assert_equal ['a';'b';'g';'e';'y';'r';'p'] 
        (Player.char_list player1));

  "update test 1">:: (fun _ -> 
      assert_equal 7 (List.length(Player.char_list player2)));

  "update test 2">:: (fun _ -> assert_equal 7 
                         (List.length(Player.char_list player3)));
  "update test 3">:: (fun _ -> assert_equal 7 
                         (List.length(Player.char_list player4))); 
  "update_turn test 1" >:: (fun _ -> assert_equal false 
                               ((Player.update_turn player5).turn));
  "update_turn test 2" >:: (fun _ -> assert_equal true 
                               ((Player.update_turn player6).turn)); 

  (* init_state_second tests *)
  "init_state_second test 1">:: (fun _-> 
      assert_equal 0 (Player.init_state_second.score));
  "init_state_second test 2">:: (fun _->
      assert_equal 7 (List.length Player.init_state_second.letters));
  "init_state_second test 3">:: (fun _->
      assert_equal false (Player.init_state_second.turn));
  "init_state_second test 4">:: (fun _->
      assert_equal "Player 2" (Player.init_state_second.turn_message));

  (* Count score tests *)
  "count_score test 1">:: (fun _->
      assert_equal 1 (Player.count_score "a" 0 []));
  "count_score test 2">:: (fun _-> 
      assert_equal 1 (Player.count_score "a" 0 [("empty", None)]));
  "count_score test 3">:: (fun _->
      assert_equal 3 (Player.count_score "a" 0 [("TW", None)]));
  "count_score test 4">:: (fun _->
      assert_equal 2 (Player.count_score "a" 0 [("DW", None)]));
  "count_score test 5">:: (fun _->
      assert_equal 3 (Player.count_score "a" 0 [("TL", (Some 'a'))]));
  "count_score test 6">:: (fun _->
      assert_equal 2 (Player.count_score "a" 0 [("DL", (Some 'a'))]));
  "count_score test 7">:: (fun _->
      assert_equal 24 (Player.count_score "zen" 0 [("DW", None)]));
  "count_score test 7">:: (fun _->
      assert_equal 36 (Player.count_score "zen" 0 [("TW", None)]));

  (* create_tiles tests *)
  "create a tile" >:: (fun _ ->
      assert_equal [('a',1);('b',1);('c',1)] 
        (create_all_tiles 'a' 1 1 [('b',1);('c',1)]));

  (* duplicate_check tests *)
  "no duplicates" >:: (fun _ ->
      assert_equal ['a';'b';'c'] (duplicate_check 0 ['a';'b';'c'] 'k'));

  "duplicates found" >:: (fun _ ->
      assert_equal ['k';'k';'k';'k';'k';'a';'b';'c'] 
        (duplicate_check 5 ['a';'b';'c'] 'k'));

  (* filter tests *)
  "filter chars 1" >:: (fun _ ->
      assert_equal [] (filter ['a';'b'] []));

  "filter chars 2" >:: (fun _ ->
      assert_equal ['a';'c'] (filter ['b'] ['a';'b';'c']));

  "filter chars 3" >:: (fun _ ->
      assert_equal ['a';'c'] (filter [] ['a';'c']));

  "filter chars 4" >:: (fun _ ->
      assert_equal ['a';'c'] (filter ['b';'d';'e'] ['a';'b';'c']));

  "filter chars 5" >:: (fun _ ->
      assert_equal ['a';'c'] (filter ['b';'f';'z'] ['a';'c']));

  (* special_score tests *)
  "special score empty" >:: (fun _ ->
      assert_equal (0,1) (special_score [] (0,1)));

  "special score DW" >:: (fun _ ->
      assert_equal (0,2) (special_score [("DW", None)] (0,1)));

  "special score TW" >:: (fun _ ->
      assert_equal (0,3) (special_score [("TW", None)] (0,1)));

  "special score DW + TW" >:: (fun _ ->
      assert_equal (0,6) (special_score [("DW", None); ("TW", None)] (0,1)));

  "special score DL" >:: (fun _ ->
      assert_equal (1,1) (special_score [("DL", Some 'a')] (0,1)));

  "special score TL" >:: (fun _ ->
      assert_equal (20,1) (special_score [("TL", Some 'q')] (0,1)));

  "special score mix 1" >:: (fun _ ->
      let lst = [("DW", None); ("DW", None); ("DL", Some 'v'); ("TL", Some 'z')] 
      in 
      assert_equal (24,4) (special_score lst (0,1)));

  "special score mix 2" >:: (fun _ ->
      let lst = [("TW", None); ("DW", None); ("DL", Some 'a'); ("TL", Some 'k')] 
      in 
      assert_equal (11,6) (special_score lst (0,1)));
]

(* Simulate mini mutable scrabble bag with this. *)
let mini_tiles = create_all_tiles 'a' 1 9 [] |> create_all_tiles 'b' 3 2 

let mini_bag = ref mini_tiles

(* READ: [re_mini_bag lst] is a slightly modiefied copy of [re_bag list] from 
   [Player] module for a mini version of scrabble bag. Since mutable data 
   structures are hard to test with other testing going on in this file, a copy 
   is made. *)
let rec re_mini_bag list = 
  match list with 
  | [] -> ()
  | h :: t -> let score = ScoreMap.find h scores in 
    mini_bag := ((h, score) :: !mini_bag); 
    re_mini_bag t

let player_for_swap = init_state_first

let mutability_tests = [

  (* Rebag tests *)
  "re_bag test 1">:: (fun _->
      assert_equal 11 (re_mini_bag []; List.length !mini_bag));

  "re_bag test 2">:: (fun _->
      assert_equal 12 (re_mini_bag ['a']; List.length !mini_bag));

  "swap a letter" >:: (fun _ ->
      let letters_len = List.length player_for_swap.letters in 
      let letter = List.hd player_for_swap.letters in
      let swapped = (swap [letter] player_for_swap) in 
      assert_equal letters_len (List.length swapped.letters));

  "swap multiple letters" >:: (fun _ ->
      let letters_len = List.length player_for_swap.letters in 
      let chars = List.tl player_for_swap.letters in
      let swapped = (swap chars player_for_swap) in 
      assert_equal letters_len (List.length swapped.letters));
]

let suite =
  "test suite for A6"  >::: List.flatten [
    scrabble_tests;
    player_tests;
    mutability_tests;
  ]

let _ = run_test_tt_main suite