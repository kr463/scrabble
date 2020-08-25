
type t = {
  score: int;
  letters: char list;
  turn: bool;
  turn_message: string;
}

module ScoreMap = Map.Make(Char)
open ScoreMap

(** [char_scores] is the map that has scrabble letters to scores mapping. *)
type char_scores = int ScoreMap.t

let scores = ScoreMap.(
    empty 
    |> add 'a' 1 
    |> add 'b' 3 
    |> add 'c' 3 
    |> add 'd' 2 
    |> add 'e' 1 
    |> add 'f' 4 
    |> add 'g' 2 
    |> add 'h' 4 
    |> add 'i' 1
    |> add 'j' 8 
    |> add 'k' 5 
    |> add 'l' 1 
    |> add 'm' 3 
    |> add 'n' 1 
    |> add 'o' 1 
    |> add 'p' 3 
    |> add 'q' 10 
    |> add 'r' 1 
    |> add 's' 1 
    |> add 't' 1 
    |> add 'u' 1 
    |> add 'v' 4 
    |> add 'w' 4 
    |> add 'x' 8 
    |> add 'y' 4 
    |> add 'z' 10 
  )

type tiles = (char * int) list

let rec create_all_tiles character score num list = 
  if num = 0 then list else 
    create_all_tiles character score (num - 1) ((character, score) :: list)


let all_tiles = create_all_tiles 'a' 1 9 [] 
                |> create_all_tiles 'b' 3 2 
                |> create_all_tiles 'c' 3 2 
                |> create_all_tiles 'd' 2 4 
                |> create_all_tiles 'e' 1 12
                |> create_all_tiles 'f' 4 2 
                |> create_all_tiles 'g' 2 3 
                |> create_all_tiles 'h' 4 2
                |> create_all_tiles 'i' 1 9 
                |> create_all_tiles 'j' 8 1 
                |> create_all_tiles 'k' 5 1 
                |> create_all_tiles 'l' 1 4 
                |> create_all_tiles 'm' 3 2 
                |> create_all_tiles 'n' 1 6
                |> create_all_tiles 'o' 1 8 
                |> create_all_tiles 'p' 3 2 
                |> create_all_tiles 'q' 10 1
                |> create_all_tiles 'r' 1 6 
                |> create_all_tiles 's' 1 4 
                |> create_all_tiles 't' 1 6
                |> create_all_tiles 'u' 1 4 
                |> create_all_tiles 'v' 4 2 
                |> create_all_tiles 'w' 4 2 
                |> create_all_tiles 'x' 8 1 
                |> create_all_tiles 'y' 4 2 
                |> create_all_tiles 'z' 10 1


let bag = ref all_tiles


let rec rnd_chars list num acc = 
  let length = List.length list in 
  if num > 0 then (
    Random.self_init ();
    let char = fst (List.nth list (Random.int length)) in 
    let b = List.remove_assoc char list in
    bag := b;
    rnd_chars b (num-1) (char::acc)
  ) else acc


let init_state_first = {
  score = 0;
  letters = rnd_chars !bag 7 [];
  turn = true;
  turn_message = "Player 1"
}


let init_state_second = {
  score = 0;
  letters = rnd_chars !bag 7 [];
  turn = false;
  turn_message = "Player 2"
}


let rec duplicate_check length filtered char = 
  if length = 0 then filtered else
    duplicate_check (length - 1) (char :: filtered) char


let rec filter used_chars player_letters = 
  match used_chars with 
  | [] -> player_letters
  | h :: t -> 
    begin
      if List.mem h player_letters then
        let filtered = List.filter (fun x -> h <> x) player_letters in (
          begin
            if List.length filtered = List.length player_letters-1 
            then filter t filtered else
              filter t (duplicate_check ((List.length player_letters-1) - 
                                         (List.length filtered)) filtered h)
          end

        ) else filter t player_letters
    end


let rec special_score list (acc1, acc2) = 
  match list with  
  | [] -> (acc1, acc2)
  | (name, None) :: t -> 
    begin 
      match name with 
      | "empty" -> special_score t (acc1, acc2)
      | "TW" -> special_score t (acc1, acc2 * 3)
      | "DW" -> special_score t (acc1, acc2 * 2)
      | _ -> failwith "impossible" 
    end
  | (name, Some v) :: t -> 
    begin 
      match name with 
      | "TL" -> special_score t (acc1 + ((ScoreMap.find v scores) * 2), acc2)
      | "DL" -> special_score t (acc1 + (ScoreMap.find v scores), acc2)
      | _ -> failwith "impossible" 
    end


let rec count_score word acc list = 
  let length = String.length word in 
  match word with
  | "" -> let (plus, times) = special_score list (0, 1) in 
    ((acc + plus) * times)
  | s -> count_score (String.sub s 1 (length - 1)) 
           ((acc + (ScoreMap.find (s.[0]) scores))) list


let update word used_chars state list edge_score = {
  score =  state.score + (count_score word 0 list) + edge_score;
  letters = rnd_chars !bag (8-(8-List.length(used_chars))) 
      (filter used_chars state.letters);
  turn = false;
  turn_message = state.turn_message
}


let rec re_bag list = 
  match list with 
  | [] -> ()
  | h :: t -> let score = ScoreMap.find h scores in 
    bag := ((h, score) :: !bag); 
    re_bag t


let swap characters state = 
  re_bag characters;
  update "" characters state [] 0


let update_turn player = {
  player with turn = not (player.turn);
}

(** [char_list player] is the list of letters that player [player] has. *)
let char_list player = 
  player.letters