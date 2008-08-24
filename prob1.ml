(*
  If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

  Find the sum of all the multiples of 3 or 5 below 1000.
*)

(* let rec loop i accum =
  if i <= 0 then accum
  else if (i mod 3) = 0 || (i mod 5) = 0 then loop (i-1) (accum + i)
  else loop (i-1) accum
in
  print_int (loop 999 0) *)

open Misc

let _ =
  print_int (List.fold_left (+) 0 (List.filter (fun x -> (x mod 3) = 0 || (x mod 5) = 0) (range 1 999)));
  print_newline ()
