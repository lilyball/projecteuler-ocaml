(*
  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

  What is the 10001st prime number?
*)

let sieve_size = 200000

type primality = Prime | Composite | Unknown

let sieve = Array.make sieve_size Unknown

let _ =
  (* mark 0 and 1 as non-prime *)
  sieve.(0) <- Composite;
  sieve.(1) <- Composite

let mark_prime i =
  let rec mark_prime_aux i inc =
    if i < sieve_size then begin
      sieve.(i) <- Composite;
      mark_prime_aux (i + inc) inc
    end in
  if sieve.(i) = Unknown then begin
    sieve.(i) <- Prime;
    mark_prime_aux (2 * i) i
  end

let find_prime n =
  let rec find_prime_aux i pi =
    match sieve.(i) with
      Prime | Unknown -> begin
        (* it's a prime *)
        mark_prime i;
        if pi = n then i
        else find_prime_aux (succ i) (succ pi)
      end
    | Composite -> find_prime_aux (succ i) pi in
  find_prime_aux 2 1

let _ =
  print_int (find_prime 10001); print_newline ()
