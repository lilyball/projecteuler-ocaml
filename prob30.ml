(*
  http://projecteuler.net/index.php?section=problems&id=30

  Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:

    1634 = 1⁴ + 6⁴ + 3⁴ + 4⁴
    8208 = 8⁴ + 2⁴ + 0⁴ + 8⁴
    9474 = 9⁴ + 4⁴ + 7⁴ + 4⁴

  As 1 = 1⁴ is not a sum it is not included.

  The sum of these numbers is 1634 + 8208 + 9474 = 19316.

  Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
*)

let digits n =
  let rec aux n acc =
    if n = 0 then acc
    else aux (n / 10) ((n mod 10) :: acc) in
  aux n []

let power_sum pow l =
  List.fold_left (fun acc n -> acc + (int_of_float ((float n) ** (float pow)))) 0 l

let matching_power_sum pow n =
  let filt = (fun x -> x <> 0) in
  let digs = List.filter filt (digits n) in
  let sum = power_sum pow digs in
  let sum_digs = List.filter filt (digits sum) in
  if List.sort compare digs = List.sort compare sum_digs then
    Some sum
  else None

let unwrap x =
  match x with
    None -> invalid_arg "unwrap"
  | Some x -> x

let _ =
  let nums = ref [] in
  for a = 0 to 9 do
    for b = a to 9 do
      for c = b to 9 do
        for d = c to 9 do
          for e = d to 9 do
            for f = e to 9 do
              for g = f to 9 do
                let num = (((((a * 10 + b) * 10 + c) * 10 + d) * 10 + e) * 10 + f)* 10 + g in
                if num > 1 then
                  match matching_power_sum 5 num with
                    None -> ()
                  | Some x -> print_int x; print_newline (); nums := x :: !nums
              done
            done
          done
        done
      done
    done
  done;
  print_string "sum: "; print_int (List.fold_left (+) 0 !nums); print_newline ()
