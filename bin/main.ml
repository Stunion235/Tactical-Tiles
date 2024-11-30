open Final

let rec ask_mode () =
  print_endline
    "_\n\
     What game mode do you want to play? Type the corresponding number.\n\
     1. Normal, without measuring your time\n\
     2. Measure the time you take\n\
     3. Race the clock\n\
     4. Color mode (add color to make aligning tiles easier)";
  try
    let x = int_of_string (input_line stdin) in
    if x > 0 && x < 5 then x else ask_mode ()
  with Failure _ -> ask_mode ()

let () =
  if not (Array.exists (fun x -> x = "skip") Sys.argv) then (
    print_endline
      "\n\
       Welcome to the Humpback Hackers' Tactical Tiles!\n\
       ________________________________________________\n";
    Unix.sleepf 1.5;
    print_endline "Press enter to continue.";
    ignore (input_char stdin));
  let mode = ask_mode () in
  Driver.main mode
