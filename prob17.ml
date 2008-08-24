(*
  http://projecteuler.net/index.php?section=problems&id=17
  
  If the numbers 1 to 5 are written out in words: one, two, three, four, five,
  then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

  If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words,
  how many letters would be used?


  NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two)
  contains 23 letters and 115 (one hundred and fifteen) contains 20 letters.
  The use of "and" when writing out numbers is in compliance with British usage.
*)

let word_constants = [
  (1, "one"); (2, "two"); (3, "three"); (4, "four"); (5, "five");
  (6, "six"); (7, "seven"); (8, "eight"); (9, "nine"); (10, "ten");
  (11, "eleven"); (12, "twelve"); (13, "thirteen"); (14, "fourteen");
  (15, "fifteen"); (16, "sixteen"); (17, "seventeen"); (18, "eighteen");
  (19, "nineteen"); (20, "twenty"); (30, "thirty"); (40, "forty");
  (50, "fifty"); (60, "sixty"); (70, "seventy"); (80, "eighty"); (90, "ninety")
]

(* words_of_int is only defined to work for positive numbers up to the thousands place *)
let words_of_int n =
  let word_of_int n =
    if n >= 100 || n <= 0 then invalid_arg "word_of_int";
    try
      List.assoc n word_constants
    with Not_found -> 
      let tens = n / 10 * 10
      and ones = n mod 10 in
      if tens > 0 && ones > 0 then
        (List.assoc tens word_constants) ^ "-" ^ (List.assoc ones word_constants)
      else if tens > 0 then
        List.assoc tens word_constants
      else
        List.assoc ones word_constants in
  let thousands = n / 1000
  and hundreds = (n mod 1000) / 100
  and tens = n mod 100 in
  let word = ref "" in
  let add_word ?label ?(sep = " ") s i =
    let word = (word_of_int i) ^ (match label with None -> "" | Some x -> " " ^ x) in
    if String.length s > 0 then s ^ sep ^ word
    else word in
  if thousands > 0 then word := add_word !word thousands ~label:"thousand";
  if hundreds > 0 then word := add_word !word hundreds ~label:"hundred";
  if tens > 0 then word := add_word !word tens ~sep:" and ";
  !word

let split_str s =
  let l = ref [] in
  for i = String.length s - 1 downto 0 do
    l := s.[i] :: !l
  done;
  !l

(* ignores spaces and hyphens *)
let count_letters word =
  List.fold_left (fun n c -> match c with ' ' | '-' -> n | _ -> succ n) 0 (split_str word)

let _ =
  let count = ref 0 in
  for i = 1 to 1000 do
    count := !count + (count_letters (words_of_int i))
  done;
  print_int !count; print_newline ()
