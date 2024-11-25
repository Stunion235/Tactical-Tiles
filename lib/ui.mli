val grid_to_string : Board.grid -> string
(**User-friendly string representation of a grid. Needed as a comparable value
   for testing.*)

val print_grid : Board.grid -> unit
(**Print [grid_to_string].*)

val simulate_solution :
  ?delay:float -> ?debug:bool -> Board.grid -> string Stack.t -> unit
(**Simulate the sliding puzzle being solved.*)
