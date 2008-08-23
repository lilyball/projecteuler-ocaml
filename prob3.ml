(*
  The prime factors of 13195 are 5, 7, 13 and 29.
  
  What is the largest prime factor of the number 600851475143 ?
*)

#load "nums.cma"
open Big_int

let prime_factors n =
  let rec loop i n factors =
    if ge_big_int i (sqrt_big_int n) then n :: factors
    else if eq_big_int (mod_big_int n i) zero_big_int then loop (succ_big_int i) (div_big_int n i) (i :: factors)
    else loop (succ_big_int i) n factors in
  loop (big_int_of_int 2) n [];;

(* the largest prime factor is always the head of the factors list *)

print_endline (string_of_big_int (List.hd (prime_factors (big_int_of_string "600851475143"))))
