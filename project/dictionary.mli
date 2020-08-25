(** Represents Scrabble English dictionary. *)

(** [EnglishWords] is a [Map] of valid english words recognized in Scrabble 
    dictionary. *)
module EnglishWords :
sig
  type elt = String.t
  type t = Set.Make(String).t
  val empty : t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val singleton : elt -> t
  val remove : elt -> t -> t
  val union : t -> t -> t
  val inter : t -> t -> t
  val disjoint : t -> t -> bool
  val diff : t -> t -> t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val subset : t -> t -> bool
  val iter : (elt -> unit) -> t -> unit
  val map : (elt -> elt) -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val for_all : (elt -> bool) -> t -> bool
  val exists : (elt -> bool) -> t -> bool
  val filter : (elt -> bool) -> t -> t
  val partition : (elt -> bool) -> t -> t * t
  val cardinal : t -> int
  val elements : t -> elt list
  val min_elt : t -> elt
  val min_elt_opt : t -> elt option
  val max_elt : t -> elt
  val max_elt_opt : t -> elt option
  val choose : t -> elt
  val choose_opt : t -> elt option
  val split : elt -> t -> t * bool * t
  val find : elt -> t -> elt
  val find_opt : elt -> t -> elt option
  val find_first : (elt -> bool) -> t -> elt
  val find_first_opt : (elt -> bool) -> t -> elt option
  val find_last : (elt -> bool) -> t -> elt
  val find_last_opt : (elt -> bool) -> t -> elt option
  val of_list : elt list -> t
  val to_seq_from : elt -> t -> elt Seq.t
  val to_seq : t -> elt Seq.t
  val add_seq : elt Seq.t -> t -> t
  val of_seq : elt Seq.t -> t
end

(** file_list contains 5 separated files which, as a whole, represent all the
    words in the English dictionary. *)
val file_list : string list

(** [read_lines file string] reads the lines in file [file] and returns
    the lines in a string [string] 
    Raises: End_of_file when the end of the file is reached *)
val read_lines : in_channel -> EnglishWords.t -> EnglishWords.t

(** [indexed_contents d filename] opens file with name [filename] from 
     directory name [d] and indexes the contents into a list of words *)
val indexed_contents :
  string -> string list -> EnglishWords.t -> EnglishWords.t

(** set1 is a set that is created from the dictionary directory containing
    all of the dictionary files in file_list as mentioned above. *)
val set1 : EnglishWords.t
