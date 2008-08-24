(*
  http://projecteuler.net/index.php?section=problems&id=21

  Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
  If d(a) = b and d(b) = a, where a  b, then a and b are an amicable pair and
  each of a and b are called amicable numbers.

  For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110;
  therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

  Evaluate the sum of all the amicable numbers under 10000.
*)

open Misc
open Big_int

let cache = Hashtbl.create 10_000

let _ =
  for i = 2 to 9_999 do
    let bi = (big_int_of_int i) in
    let sum = int_of_big_int (IntSet.fold (fun i sum -> add_big_int i sum) (proper_divisors bi) zero_big_int) in
    Hashtbl.add cache i sum
  done;
  let sum = ref 0 in
  for i = 2 to 9_999 do
    try
      let sum_a = Hashtbl.find cache i in
      let sum_b = Hashtbl.find cache sum_a in
      if i = sum_b && sum_a <> sum_b then begin
        (* we found an amicable pair *)
        sum := !sum + sum_a + sum_b;
        Hashtbl.remove cache sum_a
      end
    with Not_found -> ()
  done;
  print_int !sum; print_newline ()
