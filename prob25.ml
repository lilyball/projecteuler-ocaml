(*
  http://projecteuler.net/index.php?section=problems&id=25

  The Fibonacci sequence is defined by the recurrence relation:

    F_{n} = F_{n-1} + F_{n-2}, where F_{1} = 1 and F_2 = {1}.
  
  Hence the first 12 terms will be:

    F_{1} = 1
    F_{2} = 1
    F_{3} = 2
    F_{4} = 3
    F_{5} = 5
    F_{6} = 8
    F_{7} = 13
    F_{8} = 21
    F_{9} = 34
    F_{10} = 55
    F_{11} = 89
    F_{12} = 144
  The 12th term, F_{12}, is the first term to contain three digits.

  What is the first term in the Fibonacci sequence to contain 1000 digits?
*)

open Big_int

let upper_bound = 10_000

let cache = Array.make (succ upper_bound) None
let _ =
  cache.(0) <- Some zero_big_int;
  cache.(1) <- Some unit_big_int

let rec fib n =
  match cache.(n) with
    None ->
    let i = add_big_int (fib (pred n)) (fib (n - 2)) in
    cache.(n) <- Some i;
    i
  | Some x -> x

(* [binary_search f x y] performs a binary search of the integers from [x] to [y]
   if f returns a negative number then the current index is too low
   if f returns a positive number then the current index is too high *)
let rec binary_search (f : int -> int) (x : int) (y : int) =
  if x > y then
    failwith "binary_search failed"
  else
    let mid = (y + x) / 2 in
    let comp = f mid in
    if comp = 0 then mid
    else if comp < 0 then
      binary_search f (succ mid) y
    else
      binary_search f x (pred mid)

let _ =
  let target = 1000 in
  let n = binary_search (fun i -> compare (String.length (string_of_big_int (fib i))) target) 0 upper_bound in
  (* we've found a number with [target] digits
     now to ensure we find the _first_ one *)
  let rec find_target i =
    if String.length (string_of_big_int (fib i)) < target then (succ i)
    else find_target (pred i) in
  let n = find_target n in
  print_int n; print_newline ()
