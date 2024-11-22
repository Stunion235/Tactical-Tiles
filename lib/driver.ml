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

let print_help () =
  print_endline "Instructions:";
  Unix.sleepf 0.5;
  print_endline
    "-To make moves, type w, a, s, or d corresponding to the direction in \
     which to move a tile into the gap.";
  Unix.sleepf 0.5;
  print_endline "-Press enter after each one.";
  Unix.sleepf 0.5;
  print_endline
    "\t-Hint: think of this as moving the gap in the opposite direction.";
  Unix.sleepf 0.5;
  print_endline "-To see this help later, type 'help'.";
  Unix.sleepf 0.5;
  print_endline "-To quit, type 'stop'.\n";
  Unix.sleep 1;
  print_endline "Press enter to continue.";
  ignore (input_char stdin)

let main =
  print_endline
    "Welcome to the Humpback Hackers' Tactical Tiles!\n\
     ________________________________________________\n";
  Unix.sleepf 1.5;
  print_endline "Press enter to continue.";
  ignore (input_char stdin);
  Random.self_init ();
  let board = Board.initialize_board (ask_size ()) in
  Board.fill_board board;
  Board.shuffle board (ask_difficulty ());
  print_endline "\n\n\n";
  print_help ();
  Ui.print_grid board;
  while true do
    let m = input_line stdin in
    if String.uppercase_ascii m = "STOP" then (
      print_string "Quitting.";
      exit 0)
    else if String.uppercase_ascii m = "HELP" then print_help ()
    else (
      Board.move_tile board m;
      if List.mem (String.lowercase_ascii m) [ "w"; "a"; "s"; "d" ] then
        Ui.print_grid board;
      if Board.check_correct_board board then (
        print_endline "Success!";
        exit 0))
  done
