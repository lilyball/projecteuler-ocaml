(*
  Euler published the remarkable quadratic formula:

  n² + n + 41

  It turns out that the formula will produce 40 primes for the consecutive
  values n = 0 to 39. However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41
  is divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.

  Using computers, the incredible formula  n²  79n + 1601 was discovered, which
  produces 80 primes for the consecutive values n = 0 to 79.
  The product of the coefficients, 79 and 1601, is 126479.

  Considering quadratics of the form:

  n² + an + b, where |a|  1000 and |b|  1000

  Find the product of the coefficients, a and b, for the quadratic expression
  that produces the maximum number of primes for consecutive values of n, starting with n = 0.
*)

open Misc
open Big_int

let sieve = Sieve.make 50_000

let _ =
  (* fill the sieve *)
  Sieve.iter (fun _ -> ()) sieve

let primes a b =
  let rec aux a b n l =
    let bn = (big_int_of_int n) in
    let quad = add_int_big_int b (add_big_int (square_big_int bn) (mult_int_big_int a bn)) in
    if gt_big_int quad zero_big_int && Sieve.primality (int_of_big_int quad) sieve = Sieve.Prime then
      aux a b (succ n) (quad :: l)
    else List.rev l in
  aux a b 0 []

let count_primes a b =
  List.length (primes a b)

let _ =
  let m, n = ref 0, ref (0, 0) in
  for a = -999 to 999 do
    for b = -999 to 999 do
      let count = count_primes a b in
      if count > !m then (m := count; n := a, b)
    done
  done;
  let a, b = !n in
  Printf.printf "n^2 + %d*n + %d\n" a b;
  print_list string_of_big_int (primes a b); print_newline ();
  print_int (a * b); print_newline ()
