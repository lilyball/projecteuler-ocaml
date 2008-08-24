open Misc
open Big_int

module Int = struct
  type t = int
  let compare = compare
end

module IntMap = Map.Make(Int)

let prime_factor_map n =
  let factors = List.map int_of_big_int (prime_factors (big_int_of_int n)) in
  let mapincr map key =
    IntMap.add key (try succ (IntMap.find key map) with Not_found -> 1) map in
  List.fold_left mapincr IntMap.empty factors

(* let print_int_map map =
  print_string "[[";
  let is_first = ref true in
  IntMap.iter (fun k v -> if not !is_first then print_string "; "; is_first := false; Printf.printf "%d: %d" k v) map;
  print_string "]]"
let print_int_map_endline map =
  print_int_map map;
  print_newline () *)

let map_to_list map =
  let repeat i n =
    let rec repeat_aux i n acc =
      if n <= 0 then acc
      else repeat_aux i (pred n) (i :: acc) in
    repeat_aux i n [] in
  List.rev (IntMap.fold (fun k v l -> (repeat k v) @ l) map [])

let list_product l =
  List.fold_left ( * ) 1 l

let map_merge a b =
  let map_find i map = try IntMap.find i map with Not_found -> 0 in
  let elt_merge k v map = IntMap.add k (max v (map_find k b)) map in
  IntMap.fold elt_merge a b

let _ =
  let mmap = ref IntMap.empty in
  for i = 1 to 20 do
    mmap := map_merge !mmap (prime_factor_map i)
  done;
  print_int (list_product (map_to_list !mmap)); print_newline ()
