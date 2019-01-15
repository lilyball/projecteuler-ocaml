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

let rec collatz n =
  if n != 1 then
    if (n mod 2) = 0 then
      1 + collatz (n / 2)
    else
      1 + collatz (3 * n + 1)
  else 1

let loop n =
  let rec loop n m =
    if n = 0 then
      m
    else
      if (collatz n) > m then
        loop (n - 1) (collatz n)
      else
        loop (n - 1) m
  in loop n 0

let () =
  print_int (loop 999999)
