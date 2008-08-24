(*
  http://projecteuler.net/index.php?section=problems&id=22

  Using names.txt, a 46K text file containing over five-thousand first names,
  begin by sorting it into alphabetical order. Then working out the alphabetical value
  for each name, multiply this value by its alphabetical position in the list to obtain a name score.

  For example, when the list is sorted into alphabetical order, COLIN, which is
  worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would
  obtain a score of 938  53 = 49714.

  What is the total of all the name scores in the file?
*)

open Big_int

let names =
  let data = 
    let file = open_in "names.txt" in
    let line = input_line file in
    close_in file;
    line in
  Array.of_list (Str.split (Str.regexp_string "\",\"") (String.sub data 1 (String.length data - 2)))

let sum_name s =
  let sum = ref 0 in
  String.iter (fun c -> sum := !sum + (int_of_char c) - (int_of_char 'A') + 1) s;
  !sum

let _ =
  Array.sort compare names;
  let total = ref zero_big_int in
  for i = 1 to Array.length names do
    let idx = pred i in
    total := add_big_int !total (mult_int_big_int (sum_name names.(idx)) (big_int_of_int i))
  done;
  print_endline (string_of_big_int !total)
