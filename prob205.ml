(*
  http://projecteuler.net/index.php?section=problems&id=205

  Peter has nine four-sided (pyramidal) dice, each with faces numbered 1, 2, 3, 4.
  Colin has six six-sided (cubic) dice, each with faces numbered 1, 2, 3, 4, 5, 6.

  Peter and Colin roll their dice and compare totals: the highest total wins.
  The result is a draw if the totals are equal.

  What is the probability that Pyramidal Pete beats Cubic Colin?
  Give your answer rounded to seven decimal places in the form 0.abcdefg
*)

open Big_int

let sum_dice f die n =
  let rec sum_dice_aux f die acc =
    function
    | 0 -> f acc
    | n ->
        for i = 1 to die do
          sum_dice_aux f die (acc + i) (pred n)
        done in
  sum_dice_aux f die 0 n

let colin = Array.create 37 0

let _ =
let colin_total = ref 0 in
let colin_f x =
  for i = (succ x) to 36 do
    colin.(i) <- succ colin.(i)
  done;
  incr colin_total in
sum_dice colin_f 6 6;
let tries = ref zero_big_int
and wins = ref zero_big_int in
let attempt x =
  tries := add_int_big_int !colin_total !tries;
  wins := add_int_big_int colin.(x) !wins in
sum_dice attempt 4 9;
Printf.printf "%.7f\n" ((float_of_big_int !wins) /. (float_of_big_int !tries))
