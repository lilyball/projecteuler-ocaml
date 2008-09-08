(*
  http://projecteuler.net/index.php?section=problems&id=35

  The number, 197, is called a circular prime because all rotations
  of the digits: 197, 971, and 719, are themselves prime.

  There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

  How many circular primes are there below one million?
*)

let sieve = Sieve.make (pred 1_000_000)

let _ =
  (* fill the sieve *)
  Sieve.iter (fun _ -> ()) sieve

let is_circular x =
  (* assume the given number is already prime *)
  let rotate x =
    let s = string_of_int x in
    let l = (pred (String.length s)) in
    let s = (String.sub s l 1) ^ (String.sub s 0 l) in
    int_of_string s in
  let rec is_circular_aux x stop =  
    let x = rotate x in
    if x = stop then true
    else
      match Sieve.primality x sieve with
      | Sieve.Prime -> is_circular_aux x stop
      | Sieve.Composite -> false
      | Sieve.Unknown -> failwith "unprocessed sieve num" in
  is_circular_aux x x

let _ =  
  (* now re-iter the sieve to process each prime *)
  let primes = ref [] in
  Sieve.iter (fun x -> if is_circular x then primes := x :: !primes) sieve;
  print_int (List.length !primes); print_newline ()
