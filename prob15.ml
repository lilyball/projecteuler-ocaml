(*
  http://projecteuler.net/index.php?section=problems&id=15
  
  Starting in the top left corner of a 2×2 grid, there are 6 routes (without backtracking) to the bottom right corner.
  
  #########   #####---.   #####---.
  |   |   #   |   #   |   |   #   |
  +---+---#   +---#####   +---#---+
  |   |   #   |   |   #   |   #   |
  '---+---#   '---+---#   '---#####
  
  #---+---.   #---+---.   #---+---.
  #   |   |   #   |   |   #   |   |
  #########   #####---+   #---+---+
  |   |   #   |   #   |   #   |   |
  '---+---#   '---#####   #########
  
  How many routes are there through a 20×20 grid?
*)

open Big_int

let grid_width = 20
and grid_height = 20

let grid = Array.make_matrix (grid_width + 2) (grid_height + 2) None
let _ =
  grid.(grid_width).(grid_height) <- Some unit_big_int;
  for x = 0 to grid_width + 1 do
    grid.(x).(succ grid_height) <- Some zero_big_int
  done;
  for y = 0 to grid_height do
    grid.(succ grid_width).(y) <- Some zero_big_int
  done

let rec walker x y =
  match grid.(x).(y) with
    Some x -> x
  | None ->
      let count = add_big_int (walker (succ x) y) (walker x (succ y)) in
      grid.(x).(y) <- Some count;
      count

let _ =
  print_endline (string_of_big_int (walker 0 0))
