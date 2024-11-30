val grid_to_string : Board.grid -> string
(**User-friendly string representation of a grid. Needed as a comparable value
   for testing.*)

val print_grid : Board.grid -> unit
(**Print [grid_to_string].*)

val print_grid_styled : Board.grid -> unit
(**Print the grid but with colors corresponding to the row/col. Top row and left
   col red, 2nd row and 2nd col yellow, etc.*)

val simulate_solution :
  ?delay:float -> ?debug:bool -> Board.grid -> string Stack.t -> unit
(**Simulate the sliding puzzle being solved.*)
