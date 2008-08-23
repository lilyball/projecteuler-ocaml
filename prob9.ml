(*
  A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,
  a² + b² = c²
  For example, 3² + 4² = 9 + 16 = 25 = 5².

  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  Find the product abc.
*)


(* this is extremely brute-force. It doesn't even try and calculate c from a and b. *)
let rec loop a b c =
  if a = 999 then failwith "no answer"
  else if b = 999 then loop (succ a) (succ a) (succ a)
  else if c = 999 then loop a (succ b) (succ b)
  else if (a * a) + (b * b) = (c * c) && a + b + c = 1000 then a * b * c
  else loop a b (succ c)

let _ =
  print_int (loop 1 1 1);
  print_newline ()
