(** Modifies game state *)

open Dictionary
open Player

(** Represents the scrabble game board type. *)
type t = char array array

(** the type [orientation] represents the orientation of a word entered into
    the game board.  *)
type orientation = Vertical | Horizontal

(** Raised when command format is not in the provided example format. *)
exception InvalidCommand

(** Raised when letters not in a player's characters set is encountered. *)
exception InvalidChars 

(** Raised when an invalid orientation set is encountered. *)
exception InvalidOrientation

(** Raised when an index outside of the board is encountered. *)
exception InvalidIndex

(** Raised when a word is placed outside of the indices of the board. *)
exception InvalidPlacement

(** Raised when a word is not valid according to the English Dictionary. *)
exception InvalidWord

(** Raised when the first word placed on the empty board does not go through
    the center (8,8). *)
exception InvalidStart

(** [empty_matrix] is an empty matrix that represents the initial state
    of the board. 
    Special characters represent special score types. 
    Key: 
        # -> Triple Letter
        * -> Triple Word
        & -> Double Letter
        $ -> Double Word
*)
val empty_matrix : t

(** [empty] is an empty scrabble game. 
    Special characters represent special score types. 
    Key: 
        # -> Triple Letter
        * -> Triple Word
        & -> Double Letter
        $ -> Double Word
*)
val empty : t

(** [empty_tutorial] is an empty scrabble game for the tutorial at the beginning
    of the game. Special characters represent special score types. 
    Key: 
        # -> Triple Letter
        * -> Triple Word
        & -> Double Letter
        $ -> Double Word
*)
val empty_tutorial : t

(** [new_start_idx idx orientation board] is the actual start_index of a word 
    being entered at [idx] based on what is on [board] in [orientation]. *)
val new_start_idx : int*int -> orientation -> t -> int * int

(** [check_dictionary set word] checks whether the give [word] is in the [set] 
    containing all of the valid English Dictionary words. *)
val check_dictionary : Dictionary.EnglishWords.t -> Dictionary.EnglishWords.elt 
  -> bool

(** [score_helper idx board h] is the tuple (bool, pair) where bool is whether 
    the current tile is empty and pair is the special tile if there are 
    any. *)
val score_helper : int*int -> t -> char -> bool * (string * char option)

(** [make_word char_list start_idx orientation board acc count bool score] is 
    the (word, intersection_in_board, score_of_edge) where [word] is made with
    [char_list] from [start_idx], [intersection_in_board] is the number of 
    instances a the word touches other words on the board and [score_of_edge] is 
    the score accummulated by adjacent words made by insertion. *)
val make_word : Player.ScoreMap.key list -> int * int -> orientation -> t ->  
  Dictionary.EnglishWords.elt -> int -> bool -> int -> 
  Dictionary.EnglishWords.elt * int * int

(** [check_placement board orientation start_idx char_list] is the tuple 
    (word, intersections, edge_score) created by combining characters in 
    [char_list] with current letters on [board], starting from [start_idx] in 
    orientation [orientation] and the number of current letters on [board] 
    the new word intersects.
    Raises: 
        [InvalidPlacement] if [char_list] has too many letters to insert
        into [board]. *)
val check_placement : t -> orientation -> int * int -> 
  Player.ScoreMap.key list -> Dictionary.EnglishWords.elt * int * int

(** [insert start_idx orientation board lst] is the tuple (board, spl_tile lst)
    where [board] is the scrabble game board after adding a word entered at 
    [start_idx] with [orientation] using letters in [lst] into [board] and
    [spl_tile lst] is the list of special score elements. 
    Requires: fst [start_idx] in 1..15 and snd [start_idx] in 1..15 *)
val insert_helper : int * int -> orientation -> t -> char list -> 
  (string * char option) list -> t * (string * char option) list

(** [insert_first board start_idx orientation char_list] is 
    [board, spl_tile lst, edge_score] with the characters in [char_list] 
    inserted starting from [start_idx] in direction [orientation].
    Raises:
    - [InvalidStart] if no letters in [char_list] are inserted into index (8,8). 
    - [InvalidWord] if the letters in [char_list] do not create a valid English 
      word. *)
val insert_first : t -> int * int -> orientation -> char list -> 
  (t * (string * char option) list) * Dictionary.EnglishWords.elt * int

(** [insert board start_idx orientation char_list] is is 
    [board, spl_tile lst, edge_score] with the characters in [char_list] 
    inserted starting from [start_idx] in direction [orientation].
    Raises: 
        [InvalidWord] if the word created by combining current letters 
        on [board] with characters in [char_list] starting from [start_idx] in 
        direction [orientation] is not a valid English word or if none of the
        letters in this word intersect with a letter already on [board]. *)
val insert : t -> int * int -> orientation -> char list -> 
  (t * (string * char option) list) * Dictionary.EnglishWords.elt * int

(** [string_to_char_list player list acc] is a character list after converting 
    one letter string in [list] into characters. 
    Raises [InvalidChars] if [list] has character strings not found in
    characters set of [player]. *)
val string_to_char_list : Player.t -> char list -> string list -> char list

(** [parse string board player] is the tuple (start_idx, orientation, char_list)
    after parsing the command entered by [player].
    Raises:
        [InvalidIndex] if index in [string] is out of board,
        [InvalidOrientation] if orientation is invalid in [string]. *)
val parse : string -> t -> Player.t -> (int * int) * orientation * char list

