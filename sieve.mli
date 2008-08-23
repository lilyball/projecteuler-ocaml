type sieve
type primality = Prime | Composite | Unknown
val make : int -> sieve
val size : sieve -> int
val primality : int -> sieve -> primality
val mark_prime : int -> sieve -> unit
val find_prime : int -> sieve -> int
val iter : (int -> unit) -> sieve -> unit
(** iterates over all the primes in the sieve *)
val fold : (int -> 'a -> 'a) -> 'a -> sieve -> 'a
(** equivalent to a List.fold_left with a list of all primes in the sieve *)
exception Out_of_bounds
