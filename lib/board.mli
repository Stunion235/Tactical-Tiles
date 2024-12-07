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

val initialize_board : int -> grid
(**Make an empty grid from a size*)

val fill_board : grid -> unit
(**Mutate the grid in place to its initial setup*)

val is_move_valid : grid -> string -> bool
(***Return whether or not it is valid to make a given move on a given board*)

val move_tile : grid -> string -> unit
(**Mutate a board in place to represent moving a tile. Does nothing if the move
   is invalid.*)

val to_intlistlist : grid -> int list list
(**Int list list representation of a board for tests.*)

val shuffle : grid -> string -> string Stack.t
(**Mutate a board in place to shuffle its tiles. Return a stack containing a
   list of random moves made.*)
val shuffle2 : grid -> string -> unit

val check_correct_board : grid -> bool
(*Return whether or not the board is solved*)

val copy_board : grid -> grid
(**Return a copy of a board*)
