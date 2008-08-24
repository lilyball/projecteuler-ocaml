(*
  http://projecteuler.net/index.php?section=problems&id=16
  
  2**15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.

  What is the sum of the digits of the number 2**1000?
*)

open Big_int

let large_num = power_int_positive_int 2 1000

let sum_digits n =
  let sum = ref 0 in
  String.iter (fun c -> sum := !sum + (int_of_char c) - (int_of_char '0')) (string_of_big_int n);
  !sum

let _ =
  print_int (sum_digits large_num);
  print_newline ()
