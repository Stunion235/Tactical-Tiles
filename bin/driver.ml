open Final

let rec ask_size () =
  print_endline "What board size would you like? Give an integer >1.";
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

let format_time s =
  let mins = string_of_int (s / 60) in
  let secs = string_of_int (s mod 60) in
  if s mod 60 > 9 then mins ^ ":" ^ secs else mins ^ ":0" ^ secs

(*Print instructions with an extra explanation of the current gamemode*)
let print_help mode =
  (*Pause for t secs only if not in a timed mode*)
  let sleepf_if_untimed t = if mode <> 2 && mode <> 3 then Unix.sleepf t in
  (*Wait for confirmation if not in a timed mode*)
  let enter_if_untimed () =
    if mode <> 2 && mode <> 3 then (
      print_endline "Press enter to continue.";
      ignore (input_char stdin))
  in
  print_endline "\n\n\nInstructions:";
  sleepf_if_untimed 0.5;
  print_endline
    "-To make moves, type w, a, s, or d corresponding to the direction in \
     which to move a tile into the gap.";
  sleepf_if_untimed 0.5;
  print_endline "-Press enter after each one.";
  sleepf_if_untimed 0.5;
  print_endline
    "\t-Hint: think of this as moving the gap in the opposite direction.";
  sleepf_if_untimed 0.5;
  print_endline
    "-Your goal is to get the board in a configuration where numbers increase \
     from left to right across rows, with the empty tile in the bottom right \
     corner, like this:";
  let solve3 = Board.initialize_board 3 in
  Board.fill_board solve3;
  Ui.print_grid solve3;
  enter_if_untimed ();
  print_endline "-To see this help later, type 'help'.";
  sleepf_if_untimed 0.5;
  print_endline "-To see a simulation of the solution, type 'simulate'.";
  sleepf_if_untimed 0.5;
  print_endline "-To quit, type 'stop'.\n";
  sleepf_if_untimed 1.;
  enter_if_untimed ();
  match mode with
  | 2 ->
      print_endline
        "Time Trial Mode:\n\
         -Same as normal, but after each move the game will print your current \
         time.\n\
         -When you finish, it will say how long you took.";
      print_endline "Press enter to continue.";
      ignore (input_char stdin)
  | 3 ->
      print_endline
        "Race the Clock Mode:\n\
         -At the beginning, you set a time limit.\n\
         -After each move, the game will print how much time you have left.\n\
         -When the time hits 0, the game will end after your next move.";
      print_endline "Press enter to continue.";
      ignore (input_char stdin)
  | 4 ->
      print_string "Color Mode:\n-The colors ";
      ANSITerminal.(print_string [ red ] "red");
      print_string ", ";
      ANSITerminal.(print_string [ yellow ] "yellow");
      print_string ", ";
      ANSITerminal.(print_string [ green ] "green");
      print_string ", ";
      ANSITerminal.(print_string [ cyan ] "cyan");
      print_string ", ";
      ANSITerminal.(print_string [ blue ] "blue");
      print_string ", ";
      ANSITerminal.(print_string [ magenta ] "magenta");
      print_endline
        ", and white appear to make the solution easier to find.\n\
         -In that order, the colors go from top left to bottom right in the \
         solution.\n\
         -Press enter for an example:";
      ignore (input_char stdin);
      let solve8 = Board.initialize_board 8 in
      Board.fill_board solve8;
      Ui.print_grid_styled solve8;
      print_endline "Press enter to continue.";
      ignore (input_char stdin)
  | _ -> ()

let main mode =
  let num_moves = ref 0 in
  Random.self_init ();
  let size = ref 0 in
  (match
     Array.find_opt (fun x -> String.starts_with x ~prefix:"size") Sys.argv
   with
  | None -> size := ask_size ()
  | Some s -> (
      let arg = String.sub s 4 (String.length s - 4) in
      try
        size := int_of_string arg;
        if !size > 1 then print_endline ("Automatically chose size " ^ arg ^ ".")
        else failwith "size"
      with Failure _ ->
        print_endline ("Size `" ^ arg ^ "` is invalid. Pick a size.");
        size := ask_size ()));
  let board = Board.initialize_board !size in
  Board.fill_board board;
  let diff = ref "" in
  (match
     Array.find_opt (fun x -> List.mem x [ "easy"; "medium"; "hard" ]) Sys.argv
   with
  | None -> diff := ask_difficulty ()
  | Some d ->
      diff := d;
      print_endline ("Automatically chose " ^ d ^ " difficulty."));
  let shuf_moves = Board.shuffle board !diff in
  let init_board = Board.copy_board board in
  let copy_stack (original : string Stack.t) : string Stack.t =
    let copy = Stack.create () in
    Stack.iter (fun elem -> Stack.push elem copy) original;
    copy
  in
  let moves_copy = copy_stack shuf_moves in
  if not (Array.exists (fun x -> x = "skip") Sys.argv) then (
    print_endline "\n\n\n";
    print_help mode);
  let unsolved = ref true in
  let user_moves = Stack.create () in
  let time_limit = if mode = 3 then ask_time () else 0 in
  let start_time =
    if mode = 2 || mode = 3 then (*Start stopwatch*) Unix.time () else 0.
  in
  let print_board board =
    if mode = 4 then Ui.print_grid_styled board else Ui.print_grid board
  in
  print_board board;
  while !unsolved do
    let m = input_line stdin in
    if String.uppercase_ascii m = "STOP" then (
      print_string "Quitting.";
      exit 0)
    else if String.uppercase_ascii m = "HELP" then (
      print_help mode;
      print_board board)
    else if String.uppercase_ascii m = "SIMULATE" then (
      let board_copy = Board.copy_board init_board in
      let copy_moves = copy_stack moves_copy in
      Unix.sleepf 1.0;
      print_endline "Simulating solution...";
      Unix.sleepf 2.0;
      Ui.simulate_solution board_copy copy_moves;
      Unix.sleepf 1.0;
      print_endline "Simulation complete. Returning to game...";
      Unix.sleepf 2.0;
      print_board board)
    else if String.uppercase_ascii m = "UNDO" then
      if Board.undo board user_moves then Ui.print_grid board
      else print_endline "Nothing to undo."
    else
      let moved = Board.move_tile board m in
      if List.mem (String.lowercase_ascii m) [ "w"; "a"; "s"; "d" ] then (
        incr num_moves;
        if moved then Stack.push m user_moves);
      print_board board;
      if mode = 2 then
        print_endline
          ("Current time: "
          ^ (Unix.time () -. start_time |> int_of_float |> format_time));
      if mode = 3 then (
        let time_left =
          time_limit - int_of_float (Unix.time () -. start_time)
        in
        if time_left <= 0 then (
          print_endline "Time's up!";
          exit 0)
        else print_string "Time left: ";
        if time_left < 15 then
          ANSITerminal.(print_string [ red ] (format_time time_left))
        else if time_left < 60 then
          ANSITerminal.(print_string [ yellow ] (format_time time_left))
        else print_string (format_time time_left);
        print_newline ());
      if Board.check_correct_board board then (
        print_endline "Success!";
        print_endline ("You took " ^ string_of_int !num_moves ^ " moves!");
        if mode = 2 then
          print_endline
            ("Your time was "
            ^ (Unix.time () -. start_time |> int_of_float |> format_time));
        unsolved := false)
  done

let main_2048 = ANSITerminal.(print_string [ blue ] "Mode")
