open Big_int

let print_list f l =
  print_char '[';
  begin
    match l with
      [] -> ()
    | hd :: tl ->
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

let ( <<- ) a b x = a (b x)

module IntSet = Set.Make(struct type t = big_int let compare = compare_big_int end)

(* finds the nth sub-list and returns hd * tl *)
let rec lsplit i l =
  match l with
    [] -> failwith "lsplit out of bounds"
  | hd :: tl ->
    if i = 0 then (hd, tl)
    else lsplit (pred i) tl

(* removes the nth element and returns that elt plus the list without it *)
let lremove i l =
  let rec lremove_aux i l acc =
    match l with
      [] -> failwith "lremove out of bounds"
    | hd :: tl ->
      if i = 0 then hd, List.rev (List.fold_left (fun l i -> i :: l) acc tl)
      else lremove_aux (pred i) tl (hd :: acc) in
  lremove_aux i l []

(* [range a b] returns a list of ints from [a] to [b] *)
let rec range a b =
  if a > b then []
  else a :: range (a+1) b

let combinations f l =
  let rec aux n l acc =
    if n = 0 then f acc
    else
      for i = 0 to List.length l - 1 do
        let elt, l2 = lsplit i l in
        aux (pred n) l2 (elt :: acc)
      done in
  for i = 1 to List.length l do
    aux i l []
  done

let prod l =
  List.fold_left mult_big_int unit_big_int l

let divisors n =
  if lt_big_int n unit_big_int then IntSet.empty
  else
    let factors = (prime_factors n)
    and set = ref (IntSet.singleton unit_big_int) in
    combinations (fun x -> set := IntSet.add (prod x) !set) factors;
    !set

let proper_divisors n =
  IntSet.remove n (divisors n)

let num_divisors n =
  IntSet.cardinal (divisors n)
