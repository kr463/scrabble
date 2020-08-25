open Dictionary
open Player

type t = char array array

type orientation = Vertical | Horizontal

exception InvalidCommand

exception InvalidChars 

exception InvalidOrientation

exception InvalidIndex

exception InvalidPlacement

exception InvalidWord

exception InvalidStart

let empty_matrix = 
  let empty_matrix = Array.make_matrix 15 15 ' ' in 
  empty_matrix.(0).(3) <- '&';
  empty_matrix.(0).(0) <- '*';
  empty_matrix.(0).(7) <- '*';
  empty_matrix.(0).(11) <- '&';
  empty_matrix.(0).(14) <- '*';
  empty_matrix.(1).(1) <- '$';
  empty_matrix.(1).(5) <- '#';
  empty_matrix.(1).(9) <- '#';
  empty_matrix.(1).(13) <- '$';
  empty_matrix.(2).(2) <- '$';
  empty_matrix.(2).(6) <- '&';
  empty_matrix.(2).(8) <- '&';
  empty_matrix.(2).(12) <- '$';
  empty_matrix.(3).(0) <- '&';
  empty_matrix.(3).(3) <- '$';
  empty_matrix.(3).(7) <- '&';
  empty_matrix.(3).(11) <- '$';
  empty_matrix.(3).(14) <- '&';
  empty_matrix.(4).(4) <- '$';
  empty_matrix.(4).(10) <- '$';
  empty_matrix.(5).(1) <- '#';
  empty_matrix.(5).(5) <- '#';
  empty_matrix.(5).(9) <- '#';
  empty_matrix.(5).(13) <- '#';
  empty_matrix.(6).(2) <- '&';
  empty_matrix.(6).(6) <- '&';
  empty_matrix.(6).(8) <- '&';
  empty_matrix.(6).(12) <- '&';
  empty_matrix.(7).(0) <- '*';
  empty_matrix.(7).(3) <- '&';
  empty_matrix.(7).(7) <- '$';
  empty_matrix.(7).(11) <- '&';
  empty_matrix.(7).(14) <- '*';
  empty_matrix.(8).(2) <- '&';
  empty_matrix.(8).(6) <- '&';
  empty_matrix.(8).(8) <- '&';
  empty_matrix.(8).(12) <- '&';
  empty_matrix.(9).(1) <- '#';
  empty_matrix.(9).(5) <- '#';
  empty_matrix.(9).(9) <- '#';
  empty_matrix.(9).(13) <- '#';
  empty_matrix.(10).(4) <- '$';
  empty_matrix.(10).(10) <- '$';
  empty_matrix.(11).(0) <- '&';
  empty_matrix.(11).(3) <- '$';
  empty_matrix.(11).(7) <- '&';
  empty_matrix.(11).(11) <- '$';
  empty_matrix.(11).(14) <- '&';
  empty_matrix.(12).(2) <- '$';
  empty_matrix.(12).(6) <- '&';
  empty_matrix.(12).(8) <- '&';
  empty_matrix.(12).(12) <- '$';
  empty_matrix.(13).(1) <- '$';
  empty_matrix.(13).(5) <- '#';
  empty_matrix.(13).(9) <- '#';
  empty_matrix.(13).(13) <- '$';
  empty_matrix.(14).(0) <- '*';
  empty_matrix.(14).(3) <- '&';
  empty_matrix.(14).(7) <- '*';
  empty_matrix.(14).(11) <- '&';
  empty_matrix.(14).(14) <- '*';
  empty_matrix

let empty = 
  let empty = Array.make_matrix 15 15 ' ' in 
  empty.(0).(0) <- '*';
  empty.(0).(3) <- '&';
  empty.(0).(7) <- '*';
  empty.(0).(11) <- '&';
  empty.(0).(14) <- '*';
  empty.(1).(1) <- '$';
  empty.(1).(5) <- '#';
  empty.(1).(9) <- '#';
  empty.(1).(13) <- '$';
  empty.(2).(2) <- '$';
  empty.(2).(6) <- '&';
  empty.(2).(8) <- '&';
  empty.(2).(12) <- '$';
  empty.(3).(0) <- '&';
  empty.(3).(3) <- '$';
  empty.(3).(7) <- '&';
  empty.(3).(11) <- '$';
  empty.(3).(14) <- '&';
  empty.(4).(4) <- '$';
  empty.(4).(10) <- '$';
  empty.(5).(1) <- '#';
  empty.(5).(5) <- '#';
  empty.(5).(9) <- '#';
  empty.(5).(13) <- '#';
  empty.(6).(2) <- '&';
  empty.(6).(6) <- '&';
  empty.(6).(8) <- '&';
  empty.(6).(12) <- '&';
  empty.(7).(0) <- '*';
  empty.(7).(3) <- '&';
  empty.(7).(7) <- '$';
  empty.(7).(11) <- '&';
  empty.(7).(14) <- '*';
  empty.(8).(2) <- '&';
  empty.(8).(6) <- '&';
  empty.(8).(8) <- '&';
  empty.(8).(12) <- '&';
  empty.(9).(1) <- '#';
  empty.(9).(5) <- '#';
  empty.(9).(9) <- '#';
  empty.(9).(13) <- '#';
  empty.(10).(4) <- '$';
  empty.(10).(10) <- '$';
  empty.(11).(0) <- '&';
  empty.(11).(3) <- '$';
  empty.(11).(7) <- '&';
  empty.(11).(11) <- '$';
  empty.(11).(14) <- '&';
  empty.(12).(2) <- '$';
  empty.(12).(6) <- '&';
  empty.(12).(8) <- '&';
  empty.(12).(12) <- '$';
  empty.(13).(1) <- '$';
  empty.(13).(5) <- '#';
  empty.(13).(9) <- '#';
  empty.(13).(13) <- '$';
  empty.(14).(0) <- '*';
  empty.(14).(3) <- '&';
  empty.(14).(7) <- '*';
  empty.(14).(11) <- '&';
  empty.(14).(14) <- '*';
  empty

let empty_tutorial = 
  let empty_t = Array.make_matrix 15 15 ' ' in 
  empty_t.(0).(0) <- '*';
  empty_t.(0).(3) <- '&';
  empty_t.(0).(7) <- '*';
  empty_t.(0).(11) <- '&';
  empty_t.(0).(14) <- '*';
  empty_t.(1).(1) <- '$';
  empty_t.(1).(5) <- '#';
  empty_t.(1).(9) <- '#';
  empty_t.(1).(13) <- '$';
  empty_t.(2).(2) <- '$';
  empty_t.(2).(6) <- '&';
  empty_t.(2).(8) <- '&';
  empty_t.(2).(12) <- '$';
  empty_t.(3).(0) <- '&';
  empty_t.(3).(3) <- '$';
  empty_t.(3).(7) <- '&';
  empty_t.(3).(11) <- '$';
  empty_t.(3).(14) <- '&';
  empty_t.(4).(4) <- '$';
  empty_t.(4).(10) <- '$';
  empty_t.(5).(1) <- '#';
  empty_t.(5).(5) <- '#';
  empty_t.(5).(9) <- '#';
  empty_t.(5).(13) <- '#';
  empty_t.(6).(2) <- '&';
  empty_t.(6).(6) <- '&';
  empty_t.(6).(8) <- '&';
  empty_t.(6).(12) <- '&';
  empty_t.(7).(0) <- '*';
  empty_t.(7).(3) <- '&';
  empty_t.(7).(7) <- '$';
  empty_t.(7).(11) <- '&';
  empty_t.(7).(14) <- '*';
  empty_t.(8).(2) <- '&';
  empty_t.(8).(6) <- '&';
  empty_t.(8).(8) <- '&';
  empty_t.(8).(12) <- '&';
  empty_t.(9).(1) <- '#';
  empty_t.(9).(5) <- '#';
  empty_t.(9).(9) <- '#';
  empty_t.(9).(13) <- '#';
  empty_t.(10).(4) <- '$';
  empty_t.(10).(10) <- '$';
  empty_t.(11).(0) <- '&';
  empty_t.(11).(3) <- '$';
  empty_t.(11).(7) <- '&';
  empty_t.(11).(11) <- '$';
  empty_t.(11).(14) <- '&';
  empty_t.(12).(2) <- '$';
  empty_t.(12).(6) <- '&';
  empty_t.(12).(8) <- '&';
  empty_t.(12).(12) <- '$';
  empty_t.(13).(1) <- '$';
  empty_t.(13).(5) <- '#';
  empty_t.(13).(9) <- '#';
  empty_t.(13).(13) <- '$';
  empty_t.(14).(0) <- '*';
  empty_t.(14).(3) <- '&';
  empty_t.(14).(7) <- '*';
  empty_t.(14).(11) <- '&';
  empty_t.(14).(14) <- '*';
  empty_t


let rec new_start_idx (i, j) orientation board =
  if orientation = Horizontal then
    if (j-1) < 0 then (i,j) else
    if ((board.(i).(j-1) = ' ') ||
        (board.(i).(j-1) = '*') ||
        (board.(i).(j-1) = '&') ||
        (board.(i).(j-1) = '$') ||
        (board.(i).(j-1) = '#')) 
    then (i, j) else new_start_idx (i, j-1) orientation board
  else
  if (i-1) < 0 then (i,j) else
  if ((board.(i-1).(j) = ' ') ||
      (board.(i-1).(j) = '*') ||
      (board.(i-1).(j) = '&') ||
      (board.(i-1).(j) = '$') ||
      (board.(i-1).(j) = '#')) 
  then (i, j) else new_start_idx (i-1, j) orientation board

let check_dictionary set word = 
  if EnglishWords.mem word set then true else false

let score_helper (r, c) board h = 
  match (board.(r).(c)) with 
  | ' ' -> (true, ("empty", None))
  | '*' -> (true, ("TW", None))
  | '#' -> (true, ("TL", Some h))
  | '$' -> (true, ("DW", None))
  | '&' -> (true, ("DL", Some h))
  | ch -> (false, ("char", None))

let rec make_word char_list start_idx orientation board acc count bool score = 
  if (fst start_idx = 15 || snd start_idx = 15) && List.length char_list = 0 
  then (acc, count, score) else 
  if (fst start_idx = 15 || snd start_idx = 15) && 
     List.length char_list <> 0 
  then raise InvalidIndex 
  else
  if 
    (board.(fst start_idx).(snd start_idx) = ' ') ||
    (board.(fst start_idx).(snd start_idx) = '*') ||
    (board.(fst start_idx).(snd start_idx) = '&') || 
    (board.(fst start_idx).(snd start_idx) = '$') ||
    (board.(fst start_idx).(snd start_idx) = '#') 
  then begin 
    match char_list with 
    | [] -> (acc, count, score)
    | h :: t -> 
      if bool then 
        if orientation = Vertical
        then 
          let idx = new_start_idx start_idx Horizontal board in 
          let word, c, s = (make_word [h] idx Horizontal board "" count false 0) 
          in 
          let len = String.length word in 
          if (len = 1) then 
            make_word t ((fst start_idx)+1, snd start_idx) orientation board 
              (acc^(String.make 1 h)) count true score
          else 
          if (check_dictionary set1 word) then 
            let (b, elt) = score_helper start_idx board h in 
            let word_score = (if b then count_score word 0 [elt]
                              else count_score word 0 []) in 
            make_word t ((fst start_idx)+1, snd start_idx) orientation board 
              (acc ^ (String.make 1 h)) (count+1) true (score+word_score)
          else raise InvalidWord
        else
          let idx = new_start_idx start_idx Vertical board in 
          let word, c, s = (make_word [h] idx Vertical board "" count false 0) 
          in 
          let len = String.length word in 
          if (len = 1) then
            make_word t (fst start_idx, (snd start_idx)+1) orientation board 
              (acc^(String.make 1 h)) count true score
          else
          if (check_dictionary set1 word) then 
            let (b,elt) = score_helper start_idx board h in 
            let word_score = (if b then count_score word 0 [elt]
                              else count_score word 0 []) in 
            make_word t (fst start_idx, (snd start_idx)+1) orientation board 
              (acc^(String.make 1 h)) (count+1) true (score+word_score)
          else raise InvalidWord
      else (
        if orientation = Vertical then 
          make_word t ((fst start_idx)+1, snd start_idx) orientation board 
            (acc^(String.make 1 h)) count false score
        else make_word t (fst start_idx, (snd start_idx)+1) orientation board 
            (acc^(String.make 1 h)) count false score
      ) end 
  else (
    if orientation = Vertical 
    then make_word char_list ((fst start_idx)+1, snd start_idx) orientation 
        board (acc ^ (String.make 1 board.(fst start_idx).(snd start_idx))) 
        (count+1) bool score
    else make_word char_list (fst start_idx, (snd start_idx)+1) orientation 
        board (acc^(String.make 1 board.(fst start_idx).(snd start_idx))) 
        (count+1) bool score
  )

let check_placement board orientation start_idx char_list = 
  try 
    let idx = new_start_idx ((fst start_idx-1), (snd start_idx-1)) 
        orientation board in 
    make_word char_list idx orientation board "" 0 true 0
  with 
  | InvalidIndex -> raise InvalidPlacement

let rec insert_helper start_idx orientation board char_list acc = 
  match char_list with 
  | [] -> (board, acc) 
  | h::t -> 
    begin 
      let result = score_helper ((fst start_idx)-1,(snd start_idx)-1) board h in 
      begin
        if (fst result) then 
          begin 
            board.(fst start_idx-1).(snd start_idx-1) <- h;
            begin 
              if orientation = Vertical 
              then insert_helper ((fst start_idx)+1, snd start_idx) 
                  orientation board t ((snd result) :: acc)
              else insert_helper (fst start_idx, (snd start_idx)+1) 
                  orientation board t ((snd result) :: acc) 
            end 
          end
        else 
        if orientation = Vertical 
        then insert_helper ((fst start_idx)+1, snd start_idx) 
            orientation board char_list acc
        else insert_helper (fst start_idx, (snd start_idx)+1) 
            orientation board char_list acc 
      end
    end 

let insert_first board start_idx orientation char_list = 
  let (word, count, score) = check_placement board orientation start_idx 
      char_list in 
  let length = String.length word in 
  begin 
    if orientation = Vertical then 
      begin 
        if not ((snd (start_idx) = 8) && (fst start_idx) <= 8 && 
                ((fst start_idx) + length) > 8) 
        then raise InvalidStart else
          begin
            if check_dictionary set1 word then 
              ((insert_helper start_idx orientation board char_list []), 
               word, score) 
            else raise InvalidWord 
          end 
      end
    else 
    if not ((fst (start_idx) = 8) && (snd start_idx <= 8 && 
                                      ((snd start_idx + length) > 8))) 
    then raise InvalidStart else
      begin 
        if check_dictionary set1 word then 
          ((insert_helper start_idx orientation board char_list []), 
           word, score) 
        else raise InvalidWord 
      end
  end

let insert board start_idx orientation char_list = 
  if board = empty_matrix then 
    insert_first board start_idx orientation char_list 
  else
    let (word, count, score) = check_placement board orientation start_idx 
        char_list in 
    if (check_dictionary set1 word) && (count > 0) then 
      ((insert_helper start_idx orientation board char_list []), word, score) 
    else 
    if count > 0 then raise InvalidWord
    else raise InvalidPlacement

let rec string_to_char_list player acc = function
  | [] -> List.rev acc
  | h :: t when List.mem h.[0] (Player.char_list player) -> 
    string_to_char_list player (h.[0] :: acc) t
  | _ -> raise InvalidChars

let parse string board player = 
  match String.split_on_char ' ' string with 
  | [] -> raise InvalidCommand 
  | a :: b :: c :: [] -> begin
      let start_idx = 
        begin
          match String.split_on_char ',' a with  
          | "" :: _ -> raise InvalidIndex
          | h::t::[] -> 
            begin
              let idx_1 = int_of_string h in let idx_2 = int_of_string t in 
              if idx_1 > 0 && idx_1 < 16 && idx_2 > 0 && idx_2 < 16 
              then (idx_1,idx_2) else raise InvalidIndex
            end
          | _ -> raise InvalidIndex
        end 
      in 
      let orientation = 
        begin
          match b with 
          | "vertical" -> Vertical
          | "horizontal" -> Horizontal 
          | _ -> raise InvalidOrientation
        end
      in 
      let char_list = 
        begin
          match String.split_on_char ',' c with 
          | [] | [""] -> raise InvalidChars 
          | lst -> string_to_char_list player [] lst
        end
      in 
      (start_idx, orientation, char_list)
    end
  | _ -> raise InvalidCommand

