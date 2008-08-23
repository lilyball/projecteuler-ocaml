type primality = Prime | Composite | Unknown
type sieve = primality array
exception Out_of_bounds

let make n =
  if n < 2 then invalid_arg "sieve must have at least 2 elements"
  else
    let sieve = Array.make n Unknown in
    sieve.(0) <- Composite;
    sieve.(1) <- Composite;
    sieve

let primality i sieve =
  sieve.(i)

let size sieve =
  Array.length sieve

let mark_prime i sieve =
  let rec mark_prime_aux i inc =
    if i < size sieve then begin
      sieve.(i) <- Composite;
      mark_prime_aux (i + inc) inc
    end in
  if sieve.(i) = Unknown then begin
    sieve.(i) <- Prime;
    mark_prime_aux (2 * i) i
  end

let find_prime n sieve =
  let rec find_prime_aux i pi =
    if i >= size sieve then raise Out_of_bounds
    else
      match sieve.(i) with
        Prime | Unknown -> begin
          (* it's a prime *)
          mark_prime i sieve;
          if pi = n then i
          else find_prime_aux (succ i) (succ pi)
        end
      | Composite -> find_prime_aux (succ i) pi in
  find_prime_aux 2 1
