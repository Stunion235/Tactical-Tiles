let rec ask_size () =
  print_endline "What board size would you like? Give a positive integer >1.";
  try
    let x = int_of_string (input_line stdin) in
    if x > 1 then x else ask_size ()
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

let rec ask_time () =
  print_endline
    "How much time would you like to be given? Enter a number of seconds.";
  try
    let x = int_of_string (input_line stdin) in
    if x > 0 then x else ask_time ()
  with Failure _ -> ask_time ()

let format_time s = string_of_int (s / 60) ^ ":" ^ string_of_int (s mod 60)

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

let main mode =
  Random.self_init ();
  let board = Board.initialize_board (ask_size ()) in
  Board.fill_board board;
  let moves = Board.shuffle board (ask_difficulty ()) in
  print_endline "\n\n\n";
  print_help ();
  let unsolved = ref true in
  let time_limit = if mode = 3 then ask_time () else 0 in
  let start_time =
    if mode = 2 || mode = 3 then (*Start stopwatch*) Unix.time () else 0.
  in
  Ui.print_grid board;
  while !unsolved do
    let m = input_line stdin in
    if String.uppercase_ascii m = "STOP" then (
      print_string "Quitting.";
      exit 0)
    else if String.uppercase_ascii m = "HELP" then (
      print_help ();
      Ui.print_grid board)
    else if String.uppercase_ascii m = "SIMULATE" then
      Ui.simulate_solution board moves
    else (
      Board.move_tile board m;
      if List.mem (String.lowercase_ascii m) [ "w"; "a"; "s"; "d" ] then
        Ui.print_grid board;
      if mode = 2 then
        print_endline
          ("Current time: "
          ^ (Unix.time () -. start_time |> int_of_float |> format_time));
      (if mode = 3 then
         let time_left =
           time_limit - int_of_float (Unix.time () -. start_time)
         in
         if time_left <= 0 then (
           print_endline "Time's up!";
           exit 0)
         else print_endline ("Time left: " ^ format_time time_left));
      if Board.check_correct_board board then (
        print_endline "Success!";
        if mode = 2 then
          print_endline
            ("Your time was "
            ^ (Unix.time () -. start_time |> int_of_float |> format_time));
        unsolved := false))
  done
