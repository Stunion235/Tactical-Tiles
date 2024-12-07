type tile
type grid

val make_board : unit -> grid
val add_new : grid -> grid
val turn : grid -> grid
val merge : grid -> grid
val compress : grid -> grid
val move_up : grid -> grid
val move_down : grid -> grid
val move_right : grid -> grid
val move_left : grid -> grid
val curr_state : grid -> int -> string
val to_intlistlist : grid -> int list list
val of_intarrayarray : int array array -> grid
val to_intarrayarray : grid -> int array array

