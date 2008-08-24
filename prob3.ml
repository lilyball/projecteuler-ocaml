(*
  The prime factors of 13195 are 5, 7, 13 and 29.
  
  What is the largest prime factor of the number 600851475143 ?
*)

open Misc
open Big_int

(* the largest prime factor is always the head of the factors list *)

let _ =
  print_endline (string_of_big_int (List.hd (prime_factors (big_int_of_string "600851475143"))))
