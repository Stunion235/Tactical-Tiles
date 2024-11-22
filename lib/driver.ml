let rec ask_size () =
  print_endline "What board size would you like? Give a positive integer.";
  try
    let x = int_of_string (input_line stdin) in
    if x > 0 then x else ask_size ()
  with Failure _ -> ask_size ()

let rec ask_difficulty () =
  print_endline "What difficulty would you like? Easy, medium, or hard.";
  let x = input_line stdin in
  if
    String.uppercase_ascii x = "EASY"
    || String.uppercase_ascii x = "MEDIUM"
    || String.uppercase_ascii x = "HARD"
  then x
  else ask_difficulty ()

let main =
  Random.self_init ();
  let board = Board.initialize_board (ask_size ()) in
  Board.fill_board board;
  Board.shuffle board (ask_difficulty ());
  print_endline "To stop the game in the middle, please input 'stop'";
  Ui.print_grid board;
  while true do
    let m = input_line stdin in
    if String.uppercase_ascii m = "STOP" then exit 0
    else (
      Board.move_tile board m;
      if List.mem (String.lowercase_ascii m) [ "w"; "a"; "s"; "d" ] then
        Ui.print_grid board;
      if Board.check_correct_board board then (
        print_endline "Success!";
        exit 0))
  done
