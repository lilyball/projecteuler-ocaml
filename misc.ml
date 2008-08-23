#load "nums.cma"
open Big_int

let print_list f l =
  print_char '[';
  match l with
    [] -> ()
  | hd :: tl -> begin
    print_string (f hd);
    List.iter (fun x -> print_string (", " ^ (f x))) tl
  end;
  print_char ']'

let print_int_list l =
  print_list string_of_int l

let prime_factors n =
  let rec loop i n factors =
    if gt_big_int i (sqrt_big_int n) then n :: factors
    else if eq_big_int (mod_big_int n i) zero_big_int then loop i (div_big_int n i) (i :: factors)
    else loop (succ_big_int i) n factors in
  loop (big_int_of_int 2) n []
