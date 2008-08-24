(*
  The following iterative sequence is defined for the set of positive integers:

  n  n/2 (n is even)
  n  3n + 1 (n is odd)

  Using the rule above and starting with 13, we generate the following sequence:
  13  40  20  10  5  16  8  4  2  1
  It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.
  Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.

  Which starting number, under one million, produces the longest chain?

  NOTE: Once the chain starts the terms are allowed to go above one million.
*)

open Big_int

module BigIntHashedType =
struct
  type t = big_int
  let equal = eq_big_int
  let hash n = Hashtbl.hash (int_of_big_int (mod_big_int n (big_int_of_int max_int)))
end

module BigIntHashtbl = Hashtbl.Make(BigIntHashedType)

let cache = BigIntHashtbl.create 1_000_000
let _ =
  (* the sequence always ends at 1 *)
  BigIntHashtbl.add cache unit_big_int []

let two_big_int = (big_int_of_int 2)

let is_even n =
  eq_big_int (mod_big_int n two_big_int) zero_big_int

let calc_seq n =
  let rec calc_seq_aux n l =
    if BigIntHashtbl.mem cache n then
      let seq = BigIntHashtbl.find cache n in
      List.fold_left (fun seq n -> BigIntHashtbl.replace cache n seq; n :: seq) (n :: seq) l
    else
      let n2 = (if is_even n then div_big_int n two_big_int else add_int_big_int 1 (mult_int_big_int 3 n)) in
      calc_seq_aux n2 (n :: l) in
  calc_seq_aux n []

let _ =
  let m = ref 0 and n = ref 0 in
  for i = 1 to 999_999 do
    if (i mod 500) = 0 then (print_int i; print_newline ());
    let seq = calc_seq (big_int_of_int i) in
    let num_seq = List.length seq in
    if num_seq > !m then begin
      m := num_seq;
      n := i
    end
  done;
  Printf.printf "%d: %d\n" !n !m
