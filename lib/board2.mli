type tile
(**Type of a tile*)

type grid
(**Type of a grid of tiles*)

val make_board : unit -> grid
(**Initialize a new game of 2048*)

val add_new : grid -> grid
(**Add a new 2 in grid at any random empty cell*)

val turn : grid -> grid
(**Interchange the rows and columns of grid*)

val merge : grid -> string -> grid
(**[merge g d] merges the tiles in g in direction d*)

val compress : grid -> string -> grid
(**[compress g d] compresses the tiles in g in direction d*)

val move_up : grid -> grid
(**Update the grid after an "up" move*)

val move_down : grid -> grid
(**Update the grid after a "down" move*)

val move_right : grid -> grid
(**Update the grid after a "right" move*)

val move_left : grid -> grid
(**Update the grid after a "left" move*)

val has_legal_move : grid -> bool
(*Whether or not the grid has legal moves left*)

val curr_state : grid -> string
(**Return the current state of the game. The state is either "WON", "GAME NOT
   OVER", or "LOST"*)

val make_move : grid -> string -> grid
(**Call the corresponding move function based on user input. Returns the same
   board if the input is invalid. *)

val to_intlistlist : grid -> int list list
(**Int list list representation of a board for tests.*)

val of_intarrayarray : int array array -> grid
(**Make a grid from an int array array*)

val to_intarrayarray : grid -> int array array
(**Make an int array array from a grid*)
