val print_list : ('a -> string) -> 'a list -> unit
val print_int_list : int list -> unit
val prime_factors : Big_int.big_int -> Big_int.big_int list
val ( <<- ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
module IntSet : Set.S with type elt = Big_int.big_int
val lsplit : int -> 'a list -> 'a * 'a list
val lremove : int -> 'a list -> 'a * 'a list
val range : int -> int -> int list
val combinations : ('a list -> unit) -> 'a list -> unit
val prod : Big_int.big_int list -> Big_int.big_int
val divisors : Big_int.big_int -> IntSet.t
val proper_divisors : Big_int.big_int -> IntSet.t
val num_divisors : Big_int.big_int -> int
