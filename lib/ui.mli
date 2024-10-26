type tile
(**The type of one tile*)

type grid
(**The type of a grid of tiles*)

val int_to_tile : int -> tile
(**Turn an int into a tile*)

val intarrayarray_to_grid : int array array -> grid
(**Make a grid of tiles from an int array array*)

val print_grid : grid -> unit
(**Print a user-friendly representation of a grid*)
