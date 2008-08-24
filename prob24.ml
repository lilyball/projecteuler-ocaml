(*
  http://projecteuler.net/index.php?section=problems&id=24
  
  A permutation is an ordered arrangement of objects. For example, 3124 is one
  possible permutation of the digits 1, 2, 3 and 4. If all of the permutations
  are listed numerically or alphabetically, we call it lexicographic order.
  The lexicographic permutations of 0, 1 and 2 are:

  012   021   102   120   201   210

  What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
*)

open Misc

let rec permute f l =
  match l with
    [] -> invalid_arg "permute"
  | hd :: [] -> f [hd]
  | hd :: tl ->
    for i = 0 to List.length l - 1 do
      let elt, rem = lremove i l in
      permute (fun l -> f (elt :: l)) rem
    done

let _ =
  let i = ref 1 in
  permute (fun l -> if !i = 1_000_000 then (List.iter print_int l; print_newline ()); incr i) (range 0 9)
