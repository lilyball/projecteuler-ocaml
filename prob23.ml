(*
  A perfect number is a number for which the sum of its proper divisors is exactly
  equal to the number. For example, the sum of the proper divisors of 28 would
  be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.

  A number whose proper divisors are less than the number is called deficient and
  a number whose proper divisors exceed the number is called abundant.

  As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number
  that can be written as the sum of two abundant numbers is 24. By mathematical analysis,
  it can be shown that all integers greater than 28123 can be written as the sum of
  two abundant numbers. However, this upper limit cannot be reduced any further by
  analysis even though it is known that the greatest number that cannot be expressed
  as the sum of two abundant numbers is less than this limit.

  Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
*)

open Misc
open Big_int

type classification = Perfect | Deficient | Abundant

let classify n =
  let sum = IntSet.fold (fun i n -> (int_of_big_int i) + n) (proper_divisors (big_int_of_int n)) 0 in
  if sum = n then Perfect
  else if sum < n then Deficient
  else Abundant

(* let cache = Array.create 28124 None

let _ =
  for i = 1 to 28123 do
    if (i mod 1000) = 0 then (print_int i; print_newline ());
    cache.(i) <- classify i
  done *)

let upper_bound = 28123

let abundant =
  let ary =
    Array.init (succ upper_bound) 
      (fun x -> if x mod 1000 = 0 then (print_int x; print_newline ()); classify x) in
  let l = ref [] in
  for i = upper_bound downto 1 do
    if ary.(i) = Abundant then l := i :: !l
  done;
  !l

let cache = Array.make (succ upper_bound) false

let _ =
  let rec subloop i l =
    match l with
      [] -> ()
    | hd :: tl ->
      let sum = hd + i in
      if sum <= upper_bound then cache.(sum) <- true;
      subloop i tl in
  let rec loop l =
    match l with
      [] -> ()
    | hd :: tl -> subloop hd l; loop tl in
  loop abundant

let _ =
  let sum = ref 0 in
  for i = 1 to upper_bound do
    if not cache.(i) then sum := i + !sum
  done;
  print_endline (string_of_int !sum)
