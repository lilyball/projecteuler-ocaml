type sieve
type primality = Prime | Composite | Unknown
val make : int -> sieve
val size : sieve -> int
val primality : int -> sieve -> primality
val mark_prime : int -> sieve -> unit
val find_prime : int -> sieve -> int
exception Out_of_bounds
