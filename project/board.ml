open Scrabble

let is_blank matrix (r,c) = 
  match matrix.(r).(c) with 
  | ' ' | '*' | '&' | '$' | '#' -> true 
  | _ -> false

let rec make_columns (c: int) (char:char) (matrix:Scrabble.t) r = 
  if c > 0 then 
    begin 
      (if char = '|' 
       then 
         (print_string "|";
          begin 
            match (matrix.(15-r).(15-c)) with 
            | '*' -> ANSITerminal.(print_string [Background Magenta] "   " )
            | '&' -> ANSITerminal.(print_string [Background Red] "   ")
            | '$' -> ANSITerminal.(print_string [Background Cyan] "   ") 
            | '#' -> ANSITerminal.(print_string [Background Green] "   ") 
            | ch ->  print_string (" "^(String.make 1 ch)^" ")
          end;
          make_columns (c-1) char matrix r)
       else (print_string " ---"; make_columns (c-1) char matrix r)) 
    end
  else ()

let rec make_board r c matrix = 
  if (r > 0) then 
    begin
      if ((16-r) < 10) then print_string(string_of_int(16-r)^ "  ") 
      else (print_string (string_of_int(16-r)^ " "));
      make_columns c '|' matrix r; print_string "|\n   ";
      make_columns c '-' matrix r; print_newline();
      make_board (r-1) c matrix
    end
  else ()

let main_helper matrix = 
  print_endline 
    "\n     1   2   3   4   5   6   7   8   9   10  11  12  13  14  15";
  print_string "   ";
  make_columns 15 '-' matrix 0; print_newline();
  make_board 15 15 matrix;
  print_string "Key: ";
  ANSITerminal.(print_string [Background Magenta] "   "); 
  print_string " = Triple Word, ";
  ANSITerminal.(print_string [Background Red] "   ");
  print_string " = Double Letter, ";
  ANSITerminal.(print_string [Background Cyan] "   ");
  print_string " = Double Word, ";
  ANSITerminal.(print_string [Background Green] "   ");
  print_endline " = Triple Letter\n";
