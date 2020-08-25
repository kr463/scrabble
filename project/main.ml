open Scrabble
open Board
open Player
open Dictionary

(** Raised when no letter(s) or invalid input is entered for swapping. *)
exception InvalidSwap

(** [print_list lst] prints all elements of [lst] separated by a space. *)
let rec print_list = function 
  | [] -> print_newline ()
  | h::t -> print_char h ; print_string " " ; print_list t

(** [swap_parse player] parses the letters that the player wishes to swap
    and swaps them in place of their turn. *)
let swap_parse player = 
  print_endline "Enter the characters you wish to swap, separated by commas.";
  match read_line () with 
  | exception End_of_file -> player
  | "" -> raise InvalidSwap
  | str -> 
    begin
      match String.split_on_char ',' str with 
      | [] -> Player.swap [] player
      | lst -> begin
          let list = Scrabble.string_to_char_list player [] lst in 
          ANSITerminal.(print_string [green] 
                          "You've successfully swapped tiles in place of your \
                           turn.\n");
          Player.swap list player 
        end
    end

(** [prompt_loop player display_board board] is the game loop that prompts for
    the next move. *)
let rec prompt_loop player1 player2 display_board board = 
  if display_board 
  then (Board.main_helper board; 
        let current = if player1.turn then player1 
          else player2 in 
        let () = print_endline (current.turn_message^"'s Turn") in 
        print_endline ("Current Score: " ^ string_of_int current.score))
  else ();
  let win_condition = 100 in
  let current = if player1.turn then player1 else player2 in
  let current_bool = if player1.turn then true else false in
  print_endline "Current letters:"; 
  print_list (Player.char_list current); 
  print_endline "Enter a start index (row,col), orientation (vertical or \
                 horizontal), and the ordered list of letters to insert. \n\
                 Enter 'swap' to swap letters in place of your turn or 'skip' \
                 to skip your turn";
  print_endline "For example: 3,8 horizontal l,e,t,t,e ";
  (* print_endline "Enter 'letters' to see your current letters"; *)
  match read_line () with 
  | exception End_of_file -> ()
  | word -> 
    begin
      if String.lowercase_ascii(word) = "quit" 
      then exit 0 
      (* else
         if String.lowercase_ascii(word) = "letters" then 
         (print_endline ""; 
         print_list (Player.char_list current); print_endline "";
         prompt_loop player1 player2 false board) *)
      else 
      if String.lowercase_ascii(word) = "skip" then 
        (ANSITerminal.(print_string [green] "You've successfully skipped your \
                                             turn!\n");
         if current = player1 
         then prompt_loop (Player.update_turn current) 
             (Player.update_turn player2) true board 
         else prompt_loop (Player.update_turn player1) 
             (Player.update_turn current) true board)
      else
        try
          begin
            if String.lowercase_ascii(word) = "swap" then 
              if current = player1 
              then prompt_loop (swap_parse current) 
                  (Player.update_turn player2) true board 
              else prompt_loop (Player.update_turn player1) 
                  (swap_parse current) true board 
            else
              let (idx, orientation, list) = Scrabble.parse word board current 
              in 
              let ((new_board, acc), word, score) = Scrabble.insert board idx 
                  orientation list in
              let updated_player = Player.update word list current acc score in 
              let score = updated_player.score in if score >= win_condition then 
                let () = print_endline 
                    (updated_player.turn_message ^ " has won!") 
                in exit 0
              else
              if current_bool then 
                prompt_loop updated_player (Player.update_turn player2) 
                  true new_board 
              else prompt_loop (Player.update_turn player1) updated_player 
                  true new_board
          end
        with 
        | InvalidCommand -> 
          begin 
            ANSITerminal.(print_string [red] 
                            "\nYou entered an invalid command. \
                             Please check your format and spelling.\n"); 
            prompt_loop player1 player2 false board
          end
        | InvalidChars -> 
          begin
            ANSITerminal.(print_string [red] 
                            "\nYou cannot use letters that \
                             are not in your character set.\n"); 
            prompt_loop player1 player2 false board
          end
        | InvalidOrientation -> 
          begin
            ANSITerminal.(print_string [red] "\nThe orientation must be either \
                                              horizontal or vertical.\n"); 
            prompt_loop player1 player2 false board
          end
        | InvalidIndex -> 
          begin
            ANSITerminal.(print_string [red] "\nIndices must be between 1 and \
                                              15 inclusive.\n"); 
            prompt_loop player1 player2 false board
          end
        | InvalidPlacement -> 
          begin
            ANSITerminal.(print_string [red] "\nInvalid placement.\n"); 
            prompt_loop player1 player2 false board
          end
        | InvalidWord -> 
          begin
            ANSITerminal.(print_string [red] 
                            "\nYou entered an invalid word.\n"); 
            prompt_loop player1 player2 false board
          end
        | InvalidStart -> 
          begin
            ANSITerminal.(print_string [red] "\nFirst word on the board must \
                                              touch the middle (8,8) tile. \n"); 
            prompt_loop player1 player2 false board
          end
        | InvalidSwap -> 
          begin
            ANSITerminal.(print_string [red] 
                            "\nYou must swap at least one letter. \n"); 
            prompt_loop player1 player2 false board
          end
    end

(** [tutorial2 board_tutorial] shows the second half of the tutorial where 
    player two enters a word. *)
let tutorial2 board_tutorial = 
  print_endline "The next player will enter a word that intersects at least \
                 one existing letter on the board. \n\
                 Current letters: a l n d t p i ";
  print_string "Entering ";
  ANSITerminal.(print_string [yellow] "(8,8) vertical a,n,d ");
  print_endline "will result in: ";
  board_tutorial.(8).(7) <- 'a';
  board_tutorial.(9).(7) <- 'n';
  board_tutorial.(10).(7) <- 'd';
  main_helper board_tutorial;
  ANSITerminal.(print_string [green] "Enter any key to start the game.\n");
  match read_line () with 
  | exception End_of_file -> ()
  | _ -> ()

(** [tutorial1 ()] shows the first half of the tutorial where player one
    enters the first word. *)
let tutorial1 () =
  print_endline "The first player will enter a word that goes through (8,8).\n\
                 Current letters: h l i r e v a ";
  print_string "Entering ";
  ANSITerminal.(print_string [yellow] "(8,8) horizontal l,i,v,e ");
  print_endline "will result in: ";
  let board_tutorial = empty_tutorial in 
  board_tutorial.(7).(7) <- 'l';
  board_tutorial.(7).(8) <- 'i';
  board_tutorial.(7).(9) <- 'v';
  board_tutorial.(7).(10) <- 'e';
  main_helper board_tutorial;
  ANSITerminal.(print_string [green] 
                  "Enter any key to continue or [q] to start game.\n");
  match read_line () with 
  | exception End_of_file -> ()
  | q when String.lowercase_ascii(q) = "q" -> ()
  | _ -> tutorial2 board_tutorial

(** [helper ()] is used to pattern match what the player enters after the run
    "make play" to see if they want to skip the tutorial or not. *)
let helper () = 
  match read_line () with 
  | exception End_of_file -> ()
  | y when String.lowercase_ascii(y) = "q" -> ()
  | _ -> tutorial1 ()

(** [main] begins the game by introducing rules and triggering game loop. *)
let main () = 
  ANSITerminal.(print_string [red]  
                  "\n\nWelcome to Scrabble!\n");
  print_endline "Rules: ";
  print_endline "1. This is a two player game, where players take turns adding \
                 valid English Dictionary words to the board.";
  print_endline "2. Each player will be given 7 letters to begin with, and \
                 after each turn they will be given enough tiles to maintain \
                 a total of 7.";
  print_endline "3. Each letter has a score associated with it, which will add \
                 to the player's total score.";
  print_endline "4. Players, on their turn, can insert letters in either a \
                 horizontal or vertical orientation, and at least one letter \
                 must touch an existing letter on the board.";
  print_endline "5. The first player must place their word so that one \
                 character goes through the center of the board (8,8). ";
  print_endline "6. The game is over when there are either no more possible \
                 moves, or one of the players reaches a score of 100.";
  print_endline "7. Type 'Quit' to exit game.\n";
  ANSITerminal.(print_string [green] 
                  "Would you like to see a quick tutorial?\n");
  ANSITerminal.(print_string [green] 
                  "Press any key for the tutorial or [q] to start the game.\n");
  helper ();
  let player1 = init_state_first in 
  let player2 = init_state_second in 
  prompt_loop player1 player2 true Scrabble.empty 

let () = main ()