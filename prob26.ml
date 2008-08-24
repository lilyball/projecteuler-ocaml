(*
  http://projecteuler.net/index.php?section=problems&id=26

  A unit fraction contains 1 in the numerator. The decimal representation of the unit
  fractions with denominators 2 to 10 are given:

  1/2   =  0.5
  1/3   =  0.(3)
  1/4   =  0.25
  1/5   =  0.2
  1/6   =  0.1(6)
  1/7   =  0.(142857)
  1/8   =  0.125
  1/9   =  0.(1)
  1/10  =  0.1
  Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen
  that 1/7 has a 6-digit recurring cycle.

  Find the value of d < 1000 for which 1/d contains the longest recurring cycle in
  its decimal fraction part.
*)

open Big_int
open Ratio

module RatioHashedType =
struct
  type t = ratio
  let equal = eq_ratio
  let hash x = Hashtbl.hash (string_of_ratio x)
end

module RatioHashtbl = Hashtbl.Make(RatioHashedType)

let repeating_cycle ratio =
  let cache = RatioHashtbl.create 5 in
  let rec aux ratio cycle =
    if is_integer_ratio ratio then 0
    else
      try
        cycle - RatioHashtbl.find cache ratio
      with Not_found ->
        let new_ratio =
          let times_ten_ratio = mult_int_ratio 10 ratio in
          if le_big_int_ratio unit_big_int times_ten_ratio then
            create_ratio
              (mod_big_int (numerator_ratio times_ten_ratio) (denominator_ratio times_ten_ratio))
              (denominator_ratio times_ten_ratio)
          else times_ten_ratio in
          RatioHashtbl.add cache ratio cycle;
          aux new_ratio (succ cycle) in
  aux ratio 0

let _ =
  let m, n = ref 0, ref 0 in
  for i = 2 to 999 do
    let cycle = repeating_cycle (create_ratio unit_big_int (big_int_of_int i)) in
    if cycle > !m then (m := cycle; n := i)
  done;
  Printf.printf "%d: %d\n" !n !m
