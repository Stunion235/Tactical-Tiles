let rec ask_size () =
  print_endline "What board size would you like? Give a positive integer.";
  try
    let x = int_of_string (input_line stdin) in
    if x > 0 then x else ask_size ()
  with Failure _ -> ask_size ()

let rec ask_difficulty () =
  print_endline "What difficulty would you like? Give a positive integer.";
  try
    let x = int_of_string (input_line stdin) in
    if x > 0 then x else ask_difficulty ()
  with Failure _ -> ask_difficulty ()

let main =
  let board = Board.initialize_board (ask_difficulty ()) (ask_size ()) in
  Board.fill_board board;
  Ui.print_grid board
