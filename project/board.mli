(** Represents a Scrabble board. *)

(** [is_blank matrix (r,c)] checks whether the matrix [matrix] at spot [(r,c)]
    is blank or not. *)
val is_blank: char array array -> int * int -> bool

(** [make_columns c char matrix r] prints a row of a scrabble board. 
    If [char] is '|', prints the entries of [matrix] in row [15-r] 
    separated by vertical bars. Else, prints a row of hyphens.
    Requires: 
    - [c] is in 1..15 
    - [r] is in 1..15 
    - [char] is '|' or '-' *)
val make_columns: int -> char -> Scrabble.t -> int -> unit

(** [make_board r c matrix] prints a scrabble board with dimensions [r]x[c] 
    and entries of [matrix] and numbered rows from 1..[r] 
    Requires: [r] = [c] = Array.length matrix = Array.length matrix.(0) = 15 *)
val make_board: int -> int -> Scrabble.t -> unit

(** [main_helper matrix] prints a scrabble board with entries of [matrix] and 
    numbered columns. *)
val main_helper: Scrabble.t -> unit