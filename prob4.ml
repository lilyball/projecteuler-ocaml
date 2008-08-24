(*
  A palindromic number reads the same both ways.
  The largest palindrome made from the product of two 2-digit numbers is 9009 = 91  99.

  Find the largest palindrome made from the product of two 3-digit numbers.
*)

let rec is_palindrome s =
  if String.length s <= 1 then true
  else if s.[0] = s.[String.length s - 1] then is_palindrome (String.sub s 1 (String.length s - 2))
  else false;;

let pal = ref 0;;

let _ =
  for i = 100 to 999 do
    for j = 100 to 999 do
      let prod = i * j in
      if prod > !pal && is_palindrome (string_of_int prod) then pal := prod
    done
  done;
  print_int !pal; print_newline ()
