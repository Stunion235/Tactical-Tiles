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
  print_endline
    " (add color to make aligning tiles easier)\n\
     5. 2048 Combo (4x4 slider with a game of 2048 playing simultaneously for \
     an extra challenge.)";
  try
    let x = int_of_string (input_line stdin) in
    if x > 0 && x < 6 then x else ask_mode ()
  with Failure _ -> ask_mode ()

let () =
  if Array.exists (fun x -> x = "skip") Sys.argv then
    print_endline "Intro skipped."
  else (
    print_endline "\n";
    Driver.print_type "Welcome to the " 0.06;
    ANSITerminal.(print_string [ cyan ] "H");
    Driver.print_type "umpback " 0.1;
    ANSITerminal.(print_string [ red ] "H");
    Driver.print_type "ackers' " 0.1;
    ANSITerminal.(print_string [ cyan ] "T");
    Driver.print_type "actical " 0.1;
    ANSITerminal.(print_string [ red ] "T");
    Driver.print_type "iles!" 0.1;
    print_newline ();
    let colors = ANSITerminal.[ red; yellow; green; cyan; blue; magenta ] in
    for _ = 1 to 8 do
      for c = 0 to 5 do
        ANSITerminal.print_string [ List.nth colors c ] "_";
        flush stdout;
        Unix.sleepf 0.01
      done
    done;
    print_newline ();
    Unix.sleepf 0.4;
    print_endline "Press enter to continue.";
    ignore (input_char stdin));
  let mode = ref 0 in
  (match
     Array.find_opt (fun x -> String.starts_with x ~prefix:"mode") Sys.argv
   with
  | None -> mode := ask_mode ()
  | Some m -> (
      let arg = String.sub m 4 (String.length m - 4) in
      try
        mode := int_of_string arg;
        if !mode > 0 && !mode < 6 then
          print_endline ("Automatically chose mode " ^ arg ^ ".")
        else failwith "mode"
      with Failure _ ->
        print_endline ("Mode `" ^ arg ^ "` is invalid. Pick a mode.");
        mode := ask_mode ()));
  if !mode = 5 then (
    print_endline "\n";
    print_string "This is the ";
    ANSITerminal.(print_string [ yellow ] "2048 ");
    ANSITerminal.(print_string [ cyan ] "C");
    ANSITerminal.(print_string [ magenta ] "O");
    ANSITerminal.(print_string [ green ] "M");
    ANSITerminal.(print_string [ yellow ] "B");
    ANSITerminal.(print_string [ red ] "O");
    print_string " Mode!\n";
    let colors = ANSITerminal.[ red; yellow; green; cyan; blue; magenta ] in
    for _ = 1 to 5 do
      for c = 0 to 5 do
        ANSITerminal.print_string [ List.nth colors c ] "_";
        flush stdout;
        Unix.sleepf 0.01
      done
    done;
    print_endline "\n";
    Driver.main_multitask ())
  else if !mode = 1 then (
    print_endline "\n";
    print_string "This is the ";
    ANSITerminal.(print_string [ cyan ] "N");
    ANSITerminal.(print_string [ magenta ] "O");
    ANSITerminal.(print_string [ green ] "R");
    ANSITerminal.(print_string [ yellow ] "M");
    ANSITerminal.(print_string [ red ] "A");
    ANSITerminal.(print_string [ cyan ] "L");
    print_string " Mode!\n";
    let colors = ANSITerminal.[ red; yellow; green; cyan; blue; magenta ] in
    for _ = 1 to 4 do
      for c = 0 to 5 do
        ANSITerminal.print_string [ List.nth colors c ] "_"
      done
    done;
    print_endline "\n";
    Driver.main !mode)
  else if !mode = 2 then (
    print_endline "\n";
    print_string "This is the ";
    ANSITerminal.(print_string [ cyan ] "TIME ");
    ANSITerminal.(print_string [ magenta ] "TRIAL");
    print_string " Mode! ⏱️\n";
    let colors = ANSITerminal.[ red; yellow; green; cyan; blue; magenta ] in
    for _ = 1 to 5 do
      for c = 0 to 5 do
        ANSITerminal.print_string [ List.nth colors c ] "_"
      done
    done;
    print_endline "\n";
    Driver.main !mode)
  else if !mode = 3 then (
    print_endline "\n";
    print_string "This is the ";
    ANSITerminal.(print_string [ red ] "RACE");
    ANSITerminal.(print_string [ magenta ] " the ");
    ANSITerminal.(print_string [ blue ] "CLOCK");
    print_string " Mode! 🏃 ⏰ \n";
    let colors = ANSITerminal.[ red; yellow; green; cyan; blue; magenta ] in
    for _ = 1 to 6 do
      for c = 0 to 5 do
        ANSITerminal.print_string [ List.nth colors c ] "_"
      done
    done;
    print_endline "\n";
    Driver.main !mode)
  else if !mode = 4 then (
    print_endline "\n";
    print_string "This is the ";
    ANSITerminal.(print_string [ red ] "C");
    ANSITerminal.(print_string [ magenta ] "O");
    ANSITerminal.(print_string [ yellow ] "L");
    ANSITerminal.(print_string [ blue ] "O");
    ANSITerminal.(print_string [ cyan ] "R");
    print_string " Mode! 🌈\n";
    let colors = ANSITerminal.[ red; yellow; green; cyan; blue; magenta ] in
    for _ = 1 to 5 do
      for c = 0 to 5 do
        ANSITerminal.print_string [ List.nth colors c ] "_"
      done
    done;
    print_endline "\n";
    Driver.main !mode)
