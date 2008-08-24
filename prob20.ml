(*
  http://projecteuler.net/index.php?section=problems&id=20

  n! means n x (n - 1) x ... x 3 x 2 x 1

  Find the sum of the digits in the number 100!
*)

open Big_int

let fact_big_int i =
  let rec fact_big_int_aux i acc =
    if i = 0 then acc
    else fact_big_int_aux (pred i) (mult_int_big_int i acc) in
  fact_big_int_aux i unit_big_int

let large_number = fact_big_int 100

let _ =
  let sum = ref 0 in
  String.iter (fun i -> sum := !sum + (int_of_char i) - (int_of_char '0'))
    (string_of_big_int large_number);
  print_int !sum; print_newline ()
