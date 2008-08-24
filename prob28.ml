(*
  Starting with the number 1 and moving to the right in a clockwise direction
  a 5 by 5 spiral is formed as follows:

    21 22 23 24 25
    20  7  8  9 10
    19  6  1  2 11
    18  5  4  3 12
    17 16 15 14 13

  It can be verified that the sum of both diagonals is 101.

  What is the sum of both diagonals in a 1001 by 1001 spiral formed in the same way?
*)

let spiral_size = 1001

let _ =
  let sum = ref 1 in
  let cur = ref 1 in
  for i = 1 to spiral_size / 2 do
    for j = 1 to 4 do
      cur := !cur + (i * 2);
      sum := !sum + !cur
    done
  done;
  print_int !sum; print_newline ()
