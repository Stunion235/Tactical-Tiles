let ask_size =
  print_endline "What board size would you like?";
  int_of_string (input_line stdin)

let ask_difficulty =
  print_endline "What difficulty would you like?";
  int_of_string (input_line stdin)

let main =
  let board = ref (Board.initialize_board ask_difficulty ask_size) in
  Board.fill_board !board;
  Ui.print_grid !board
