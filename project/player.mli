(** Represents player states. *)

(** The abstract type of values representing a player in the scrabble game.
    Each player has a score and a list of letter. *)
type t = {
  score : int;
  letters : char list;
  turn : bool;
  turn_message : string;
}

(* [ScoreMap] is a [Map] of letters to scores. *)
module ScoreMap :
sig
  type key = Char.t
  type 'a t = 'a Map.Make(Char).t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem : key -> 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val update : key -> ('a option -> 'a option) -> 'a t -> 'a t
  val singleton : key -> 'a -> 'a t
  val remove : key -> 'a t -> 'a t
  val merge :
    (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
  val union : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val iter : (key -> 'a -> unit) -> 'a t -> unit
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val for_all : (key -> 'a -> bool) -> 'a t -> bool
  val exists : (key -> 'a -> bool) -> 'a t -> bool
  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  val partition : (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
  val cardinal : 'a t -> int
  val bindings : 'a t -> (key * 'a) list
  val min_binding : 'a t -> key * 'a
  val min_binding_opt : 'a t -> (key * 'a) option
  val max_binding : 'a t -> key * 'a
  val max_binding_opt : 'a t -> (key * 'a) option
  val choose : 'a t -> key * 'a
  val choose_opt : 'a t -> (key * 'a) option
  val split : key -> 'a t -> 'a t * 'a option * 'a t
  val find : key -> 'a t -> 'a
  val find_opt : key -> 'a t -> 'a option
  val find_first : (key -> bool) -> 'a t -> key * 'a
  val find_first_opt : (key -> bool) -> 'a t -> (key * 'a) option
  val find_last : (key -> bool) -> 'a t -> key * 'a
  val find_last_opt : (key -> bool) -> 'a t -> (key * 'a) option
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  val to_seq : 'a t -> (key * 'a) Seq.t
  val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
  val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
  val of_seq : (key * 'a) Seq.t -> 'a t
end

(** [score] is the map of letters to scores. *)
val scores : int ScoreMap.t

(** [tiles] is the list of character and score pairings that represents a 
    scrabble bag. *)
type tiles = (char * int) list

(** [create_all_tiles character score num list] creates a bag of tiles, 
    with a specific number of letters according to the scrabble rules. *)
val create_all_tiles : 'a -> 'b -> int -> ('a * 'b) list -> ('a * 'b) list

(** [all_tiles] is the (score, number of occurrence) pair list representing the
    bag of scrabble letters. *)
val all_tiles : (char * int) list

(** [bag] is a mutable representation of the scrabble characters bag. *)
val bag : (char * int) list ref

(** [rnd_chars num acc] is a char list with [num] random chars. *)
val rnd_chars : (char * int) list -> int -> char list -> char list

(** [init_state]  is the initial state of the players when the game 
    begins. In the state, their score starts at 0 and they are given 7 
    random letters. *)
val init_state_first : t

(** [init_state_second] is the initial state of the second player when the 
    game begins. In the state, score is 0, there are 7 random letters, it
    is not this player's turn, and this player's turn message is 
    ["Player 2"]. *)
val init_state_second : t

(** [duplicate_check length filtered char] is the [filtered] list after 
    checking duplicates of [char] for [length] in [filtered]. *)
val duplicate_check : int -> 'a list -> 'a -> 'a list

(** [filter used_chars player_letters] is the list of [player_letters] after 
    [used_chars] is removed. *)
val filter : 'a list -> 'a list -> 'a list

(** [special_score list acc] is the tuple (adder,multiplier) where adder is the 
    score to be added and multipler is the multiple of the score. 
    Requires:
        [list] of special score information, 
        [acc] of initial adder and multiplier. *)
val special_score :
  (string * ScoreMap.key option) list -> int * int -> int * int

(** [count_score word acc list] is the total score a player should receive
    from creating [word], including additional points gained by placing letters
    on special tiles. *)
val count_score : string -> int -> (string * ScoreMap.key option) list -> int

(** [update used_chars state] is [state] with [state.score] updated based on 
    the word they created on the board, with the letters they used [used_chars] 
    removed from [state.letters] and replaced by new, random chars.*)
val update :
  string -> char list -> t -> (string * ScoreMap.key option) list -> int -> t

(** [re_bag list] is [bag] with letters in [list] and their corresponding 
    scores added. *)
val re_bag : ScoreMap.key list -> unit

(** [swap characters state] swaps the characters [characters] for the player 
    [state] by first putting those characters bag in the bag of letters and 
    replacing them with new ones. *)
val swap : ScoreMap.key list -> t -> t


(** [update_turn player] changes the turn of player [player] to 
    the opposite of what it currently is. *)
val update_turn : t -> t

(** [char_list player] is the list of letters that player [player] has. *)
val char_list : t -> char list
