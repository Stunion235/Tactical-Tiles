open OUnit2
open Final

(**Version of [simulate_solution] without print/sleep. Testing only.*)
let solve board shuffle_moves =
  while not (Stack.is_empty shuffle_moves) do
    let move = Stack.pop shuffle_moves in
    let inverse_move =
      match move with
      | "w" | "W" -> "s"
      | "a" | "A" -> "d"
      | "s" | "S" -> "w"
      | "d" | "D" -> "a"
      | _ -> failwith "Invalid move"
    in
    Board.move_tile board inverse_move
  done

let tests =
  [
    ( "Correctly detect invalid moves" >:: fun _ ->
      let board = Board.initialize_board 3 in
      Board.fill_board board;
      assert_equal false
        (Board.is_move_valid board "w" || Board.is_move_valid board "a") );
    ( "Correctly detect valid moves" >:: fun _ ->
      let board = Board.initialize_board 3 in
      Board.fill_board board;
      assert_equal true
        (Board.is_move_valid board "s" && Board.is_move_valid board "d") );
  ]

let qtests =
  [
    QCheck2.Test.make ~count:50
      ~name:
        "An filled board of size>1 must have exactly one empty tile and \
         exactly one tile whose value is size^2-1."
      ~print:QCheck.Print.(list int)
      QCheck2.Gen.(list_size (int_range 1 1) (int_range 2 10))
      (fun x ->
        let board = Board.initialize_board (List.nth x 0) in
        Board.fill_board board;
        let board_as_list = List.flatten (Board.to_intlistlist board) in
        (*Since QCheck's ~print prints the inputs that caused failure instead of
          the objects that failed to compare, we improvise this to print the
          board only in failure, like ~print in OUnit2's assert_equal*)
        let passed =
          List.length (List.filter (fun y -> y = -1) board_as_list) = 1
          && List.length
               (List.filter
                  (fun y -> y = (List.nth x 0 * List.nth x 0) - 1)
                  board_as_list)
             = 1
        in
        if not passed then (
          Ui.print_grid board;
          List.iter print_int board_as_list);
        passed);
    QCheck2.Test.make ~count:50
      ~name:
        "Aside from the empty tile, a filled board of size>1 must have tiles \
         ranging from 1 to size^2-1."
      ~print:QCheck.Print.(list int)
      QCheck2.Gen.(list_size (int_range 1 1) (int_range 2 10))
      (fun x ->
        let board = Board.initialize_board (List.nth x 0) in
        Board.fill_board board;
        let board_as_list = List.flatten (Board.to_intlistlist board) in
        let passed =
          try
            for i = 1 to List.nth x 0 do
              if not (List.exists (fun y -> y = i) board_as_list) then
                (*Use exception to break loop*)
                failwith "number not found"
            done;
            true
          with Failure _ -> false
        in
        if not passed then (
          Ui.print_grid board;
          List.iter print_int board_as_list);
        passed);
    QCheck2.Test.make ~count:20
      ~name:
        "Solving a board puts it in its initial state. Crucial to test a bug we\n\
        \        had with solving."
      ~print:QCheck.Print.(list int)
      QCheck2.Gen.(list_size (int_range 1 1) (int_range 2 5))
      (fun x ->
        let board = Board.initialize_board (List.nth x 0) in
        Board.fill_board board;
        let initial = Board.to_intlistlist board in
        let shuffle_moves = Board.shuffle board "easy" in
        solve board shuffle_moves;
        let final = Board.to_intlistlist board in
        let passed = initial = final in
        if not passed then Ui.print_grid board;
        passed);
    QCheck2.Test.make ~count:8
      ~name:
        "An unfilled board is NOT reported as solved by [check_correct_board]."
      ~print:QCheck.Print.(list int)
      QCheck2.Gen.(list_size (int_range 1 1) (int_range 2 9))
      (fun x ->
        let board = Board.initialize_board (List.nth x 0) in
        let passed = not (Board.check_correct_board board) in
        if not passed then Ui.print_grid board;
        passed);
    QCheck2.Test.make ~count:8
      ~name:
        "A filled board is reported as solved by [check_correct_board] before\n\
        \        any moves are made."
      ~print:QCheck.Print.(list int)
      QCheck2.Gen.(list_size (int_range 1 1) (int_range 2 9))
      (fun x ->
        let board = Board.initialize_board (List.nth x 0) in
        Board.fill_board board;
        let passed = Board.check_correct_board board in
        if not passed then Ui.print_grid board;
        passed);
    QCheck2.Test.make ~count:8
      ~name:
        "An invalid move on a new board does not change it from its initial\n\
        \        state."
      ~print:QCheck.Print.(list int)
      QCheck2.Gen.(list_size (int_range 1 1) (int_range 2 9))
      (fun x ->
        let board = Board.initialize_board (List.nth x 0) in
        Board.fill_board board;
        let initial = Board.to_intlistlist board in
        Board.move_tile board "w" (*1st move W is never valid*);
        let final = Board.to_intlistlist board in
        let passed = initial = final in
        if not passed then Ui.print_grid board;
        passed);
    QCheck2.Test.make ~count:8
      ~name:"A valid move on a new board takes it out of its initial state."
      ~print:QCheck.Print.(list int)
      QCheck2.Gen.(list_size (int_range 1 1) (int_range 2 9))
      (fun x ->
        let board = Board.initialize_board (List.nth x 0) in
        Board.fill_board board;
        let initial = Board.to_intlistlist board in
        Board.move_tile board "s" (*1st move S is always valid*);
        let final = Board.to_intlistlist board in
        let passed = initial <> final in
        if not passed then Ui.print_grid board;
        passed);
    QCheck2.Test.make ~count:8 ~name:"Simulation restores original board"
      (QCheck2.Gen.pair
         (QCheck2.Gen.int_range 3 10)
         (QCheck2.Gen.oneofl [ "easy"; "medium"; "hard" ]))
      (fun (size, difficulty) ->
        let board = Board.initialize_board size in
        Board.fill_board board;
        let init_board = Board.copy_board board in
        let moves = Board.shuffle board difficulty in
        Ui.simulate_solution ~delay:0.0 ~debug:true board moves;
        Board.to_intarrayarray board = Board.to_intarrayarray init_board);
    QCheck2.Test.make ~count:8 ~name:"Simulation uses all shuffle moves"
      (QCheck2.Gen.pair
         (QCheck2.Gen.int_range 3 10)
         (QCheck2.Gen.oneofl [ "easy"; "medium"; "hard" ]))
      (fun (size, difficulty) ->
        let board = Board.initialize_board size in
        Board.fill_board board;
        let moves = Board.shuffle board difficulty in
        let move_count_before = Stack.length moves in
        Ui.simulate_solution ~delay:0.0 ~debug:true board moves;
        (* After simulation, the stack should be empty *)
        Stack.length moves = 0 && move_count_before > 0);
    QCheck2.Test.make ~count:8
      ~name:"Simulation consistently solves puzzle from any state"
      (QCheck2.Gen.pair
         (QCheck2.Gen.int_range 3 10)
         (QCheck2.Gen.oneofl [ "easy"; "medium"; "hard" ]))
      (fun (size, difficulty) ->
        let board = Board.initialize_board size in
        Board.fill_board board;
        let moves = Board.shuffle board difficulty in
        let init_board = Board.copy_board board in
        let random_moves = 10 in
        for _ = 1 to random_moves do
          let direction = List.nth [ "w"; "a"; "s"; "d" ] (Random.int 4) in
          Board.move_tile board direction
        done;
        Ui.simulate_solution ~delay:0.0 ~debug:true init_board moves;
        let solved_board = Board.initialize_board size in
        Board.fill_board solved_board;
        Board.to_intarrayarray init_board = Board.to_intarrayarray solved_board);
  ]

let _ =
  Random.self_init ();
  run_test_tt_main
    ("tests" >::: tests @ QCheck_runner.to_ounit2_test_list qtests)
