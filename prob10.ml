(*
  The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

  Find the sum of all the primes below two million.
*)

open Big_int

let sieve = Sieve.make 2000000
let _ =
  print_endline (string_of_big_int (Sieve.fold (fun a b -> add_big_int (big_int_of_int a) b) zero_big_int sieve))
