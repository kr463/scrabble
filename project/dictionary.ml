module EnglishWords = Set.Make(String)

let set = EnglishWords.empty


let file_list = ["p1.txt"; "p2.txt"; "p3.txt"; "p4.txt"; "p5.txt"]


let rec read_lines file set = 
  try (
    let line = Stdlib.input_line file in 
    let newSet = EnglishWords.add line set in read_lines file newSet
  ) with 
  | End_of_file -> set


let rec indexed_contents d file_list set = 
  match file_list with 
  | [] -> set
  | h :: t -> 
    let file = Stdlib.open_in (String.concat Filename.dir_sep [d;h]) in 
    indexed_contents d t (read_lines file set)


let set1 = indexed_contents "dictionary" file_list set