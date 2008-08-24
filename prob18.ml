(*
  By starting at the top of the triangle below and moving to adjacent numbers on the row below,
  the maximum total from top to bottom is 23.

     3
    7 5
   2 4 6
  8 5 9 3

  That is, 3 + 7 + 4 + 9 = 23.

  Find the maximum total from top to bottom of the triangle below:

                              75
                            95  64
                          17  47  82
                        18  35  87  10
                      20  04  82  47  65
                    19  01  23  75  03  34
                  88  02  77  73  07  63  67
                99  65  04  28  06  16  70  92
              41  41  26  56  83  40  80  70  33
            41  48  72  33  47  32  37  16  94  29
          53  71  44  65  25  43  91  52  97  51  14
        70  11  33  28  77  73  17  78  39  68  17  57
      91  71  52  38  17  14  91  43  58  50  27  29  48
    63  66  04  68  89  53  67  30  73  16  69  87  40  31
  04  62  98  27  23  09  70  98  73  93  38  53  60  04  23

  NOTE: As there are only 16384 routes, it is possible to solve this problem by trying every route.
  However, Problem 67, is the same challenge with a triangle containing one-hundred rows;
  it cannot be solved by brute force, and requires a clever method! ;o)
*)

(* it seems the best thing to do here is the same thing I did in problem 15
   which is to start at the top and memoize the maximum value at each node.
   This means each time I hit a node I've seen before, I already know the
   maximum valued route starting from that point. The hardest problem here
   is going to be the data structure to hold this information. *)

(* a lot of the code in here is superfluous to the actual problem, such as print_triangle.
   everything that's unnecessary has been commented out *)

type node = Leaf of int | Node of int * node * node

let triangle =
  let rev_orig_triangle = List.rev [
    [75];
    [95; 64];
    [17; 47; 82];
    [18; 35; 87; 10];
    [20; 04; 82; 47; 65];
    [19; 01; 23; 75; 03; 34];
    [88; 02; 77; 73; 07; 63; 67];
    [99; 65; 04; 28; 06; 16; 70; 92];
    [41; 41; 26; 56; 83; 40; 80; 70; 33];
    [41; 48; 72; 33; 47; 32; 37; 16; 94; 29];
    [53; 71; 44; 65; 25; 43; 91; 52; 97; 51; 14];
    [70; 11; 33; 28; 77; 73; 17; 78; 39; 68; 17; 57];
    [91; 71; 52; 38; 17; 14; 91; 43; 58; 50; 27; 29; 48];
    [63; 66; 04; 68; 89; 53; 67; 30; 73; 16; 69; 87; 40; 31];
    [04; 62; 98; 27; 23; 09; 70; 98; 73; 93; 38; 53; 60; 04; 23]
  ] in
  let rec process_row ?old_row row remainder =
    let new_row =
      match old_row with
        None -> List.map (fun i -> Leaf i) row
      | Some nodes ->
          let res, _ = List.fold_left
            (fun (row, old) i ->
              match old with
                a :: b :: rest -> Node (i, a, b) :: row, b :: rest
              | _ -> failwith "ran out of elements in process_row")
            ([], nodes) row in
          List.rev res in
    match remainder with
      [] -> new_row
    | hd :: tl -> process_row ~old_row:new_row hd tl in
  List.hd (process_row (List.hd rev_orig_triangle) (List.tl rev_orig_triangle))

(* (* [f i d] where d is the depth into the triangle *)
  let iter_triangle_breadth (f : int -> int -> unit) tri =
  let rec loop f l =
    match l with
      [] -> ()
    | (Leaf i, depth, _) :: rest -> begin
        f i depth;
        loop f rest
      end
    | (Node (i, a, b), depth, left) :: rest -> begin
        f i depth;
        if left then
          loop f (rest @ [a, succ depth, true; b, succ depth, false])
        else
          loop f (rest @ [b, succ depth, false])
      end in
  loop f [tri, 1, true]

let iter_triangle_depth (f : int -> unit) tri =
  let rec loop f l =
    match l with
      [] -> ()
    | (Leaf i, _) :: rest -> begin
        f i;
        loop f rest
      end
    | (Node (i, a, b), right) :: rest -> begin
        f i;
        loop f ((a, true) :: (if right then (b, false) :: rest else rest))
      end in
  loop f [tri, true]

let fold_triangle_depth f init tri =
  let acc = ref init in
  iter_triangle_depth (fun node -> acc := f !acc node) tri;
  !acc

let triangle_depth tri =
  let rec loop tri depth =
    match tri with
      Leaf _ -> depth
    | Node (_, a, _) -> loop a (succ depth) in
  loop tri 1

(* pretty-print a triangle *)
let print_triangle tri =
  let depth = triangle_depth tri
  and max_word = fold_triangle_depth (fun i n -> max i (String.length (string_of_int n))) 0 tri
  and old_depth = ref 0
  and repeat_str s n =
    let rec repeat_str_aux s n acc =
      if n = 0 then acc
      else repeat_str_aux s (pred n) (acc ^ s) in
    repeat_str_aux s n "" in
  let pad_str s n =
    let len = String.length s in
    (if len < n then repeat_str "0" (n - len) else "") ^ s in
  let indent = (repeat_str " " max_word) in
  let print_node i d =
    if d = !old_depth then print_string indent
    else begin
      print_newline ();
      print_string (repeat_str indent (depth - d));
      old_depth := d
    end;
    print_string (pad_str (string_of_int i) max_word) in
  iter_triangle_breadth print_node tri

(* make a hashtable the size of the triangle *)
let cache = Hashtbl.create (fold_triangle_depth (fun n _ -> succ n) 0 triangle) *)
let cache = Hashtbl.create 100

let rec walk tri =
  try
    Hashtbl.find cache tri
  with Not_found ->
    match tri with
      Leaf i -> Hashtbl.add cache tri i; i
    | Node (i, a, b) ->
      let n = i + max (walk a) (walk b) in
      Hashtbl.add cache tri n;
      n

let _ =
  print_int (walk triangle); print_newline ()
