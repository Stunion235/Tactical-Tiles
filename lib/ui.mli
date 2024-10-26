type tile_value
(**The type of one tile*)

type grid
(**The type of a grid of tiles*)

val int_to_tile_value : int -> tile_value
(**Turn an int into a tile*)

val intarray_to_grid : int array array -> grid
(**Make a grid of tiles from an int array*)

val print_grid : grid -> unit
(**Print a user-friendly representation of a grid*)
