type tile
(**Type of a tile*)

type grid
(**Type of a grid of tiles*)

val find_empty : grid -> int * int
(**Return the (row,col) coordinates of the empty tile. If there are multiple
   empty tiles, one of them is returned.*)

val create_tile : int -> tile
(**Create a tile from an int*)

val tile_to_string : tile -> string
(**Printable value of the contents of a tile*)

val of_intarrayarray : int array array -> grid
(**Make a grid from an int array array*)

val to_intarrayarray : grid -> int array array
(**Make an int array array from a grid*)
