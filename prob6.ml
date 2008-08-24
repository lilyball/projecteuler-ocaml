(*
  The sum of the squares of the first ten natural numbers is,
  1² + 2² + ... + 10² = 385
  The square of the sum of the first ten natural numbers is,
  (1 + 2 + ... + 10)² = 55² = 3025
  Hence the difference between the sum of the squares of the first
  ten natural numbers and the square of the sum is 3025  385 = 2640.

  Find the difference between the sum of the squares of the first
  one hundred natural numbers and the square of the sum.
*)

open Misc
open Big_int

let sum l = List.fold_left add_big_int zero_big_int l

let range a b =
  let rec range_aux a b acc =
    if b < a then acc
    else range_aux a (pred b) ((big_int_of_int b) :: acc) in
  range_aux a b []

let sum_of_squares l =
  sum (List.map square_big_int l)

let square_of_sum l =
  square_big_int (sum l)

let diff_of_sums l =
  sub_big_int (square_of_sum l) (sum_of_squares l)

let _ =
  print_endline (string_of_big_int (diff_of_sums (range 1 100)))
