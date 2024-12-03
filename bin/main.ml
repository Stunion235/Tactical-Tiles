open Final

let rec ask_mode () =
  print_string
    "_\n\
     What game mode do you want to play? Type the corresponding number.\n\
     1. Normal\n\
     2. Time Trial (measure your time)\n\
     3. Race the Clock (finish in a time limit)\n\
     4. ";
  ANSITerminal.(print_string [ green ] "Color ");
  ANSITerminal.(print_string [ magenta ] "Mode");
  print_endline " (add color to make aligning tiles easier)";
  try
    let x = int_of_string (input_line stdin) in
    if x > 0 && x < 5 then x else ask_mode ()
  with Failure _ -> ask_mode ()

let () =
  if not (Array.exists (fun x -> x = "skip") Sys.argv) then (
    print_endline "\n";
    print_string "Welcome to the ";
    ANSITerminal.(print_string [ cyan ] "H");
    print_string "umpback ";
    ANSITerminal.(print_string [ red ] "H");
    print_string "ackers' ";
    ANSITerminal.(print_string [ cyan ] "T");
    print_string "actical ";
    ANSITerminal.(print_string [ red ] "T");
    print_string "iles!\n";
    let colors = ANSITerminal.[ red; yellow; green; cyan; blue; magenta ] in
    for _ = 1 to 8 do
      for c = 0 to 5 do
        ANSITerminal.print_string [ List.nth colors c ] "_"
      done
    done;
    print_newline ();
    Unix.sleepf 1.5;
    print_endline "Press enter to continue.";
    ignore (input_char stdin));
  let mode = ask_mode () in
  Driver.main mode
