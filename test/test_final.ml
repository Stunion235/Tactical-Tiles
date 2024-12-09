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
    ignore (Board.move_tile board inverse_move)
  done

<<<<<<< Updated upstream
let intarrayarray_to_string x =
  let acc = ref "" in
  Array.iter
    (fun r ->
      acc := !acc ^ "\n";
      Array.iter (fun c -> acc := !acc ^ string_of_int c ^ " ") r)
    x;
  !acc

=======
>>>>>>> Stashed changes
(*Slider tests*)
let tests =
  "misc tests"
  >::: [
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

let make_find_empty_test name input expected =
  name >:: fun _ ->
  let grid = Board.of_intarrayarray input in
  let actual = Board.find_empty grid in
  assert_equal expected actual ~msg:name

let find_empty_tests =
  "find empty tests"
  >::: [
         make_find_empty_test "empty tile at bottom-right"
           [| [| 1; 2; 3 |]; [| 4; 5; 6 |]; [| 7; 8; -1 |] |]
           (2, 2);
         make_find_empty_test "empty tile in the middle"
           [| [| 1; 2; 3 |]; [| 4; -1; 6 |]; [| 7; 8; 5 |] |]
           (1, 1);
         make_find_empty_test "empty tile at top-left"
           [| [| -1; 2; 3 |]; [| 4; 5; 6 |]; [| 7; 8; 9 |] |]
           (0, 0);
       ]

let make_basic_undo_test name moves expected =
  name >:: fun _ ->
  let grid = Board.initialize_board 2 in
  Board.fill_board grid;
  let test_moves = Stack.create () in
  List.iter
    (fun x ->
      Stack.push x test_moves;
      ignore (Board.move_tile grid x))
    moves;
  let actual = Board.undo grid test_moves in
  assert_equal expected actual ~msg:name ~printer:(fun x ->
      if x then "true" else "false")

let basic_undo_tests =
  "Simple undo tests"
  >::: [
         make_basic_undo_test "Undoing one valid move returns true" [ "s" ] true;
         make_basic_undo_test "Undoing one invalid move returns true" [ "w" ]
           true;
         make_basic_undo_test "Undoing no moves returns false" [] false;
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
          board only in failure, like ~printer in OUnit2's assert_equal*)
        let passed =
          List.length (List.filter (fun y -> y = -1) board_as_list) = 1
          && List.length
               (List.filter
                  (fun y -> y = (List.nth x 0 * List.nth x 0) - 1)
                  board_as_list)
             = 1
        in
        if not passed then (
          board |> Board.to_intarrayarray |> intarrayarray_to_string
          |> print_endline;
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
          board |> Board.to_intarrayarray |> intarrayarray_to_string
          |> print_endline;
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
        if not passed then
          board |> Board.to_intarrayarray |> intarrayarray_to_string
          |> print_endline;
        passed);
    QCheck2.Test.make ~count:8
      ~name:
        "An unfilled board is NOT reported as solved by [check_correct_board]."
      ~print:QCheck.Print.(list int)
      QCheck2.Gen.(list_size (int_range 1 1) (int_range 2 9))
      (fun x ->
        let board = Board.initialize_board (List.nth x 0) in
        let passed = not (Board.check_correct_board board) in
        if not passed then
          board |> Board.to_intarrayarray |> intarrayarray_to_string
          |> print_endline;
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
        if not passed then
          board |> Board.to_intarrayarray |> intarrayarray_to_string
          |> print_endline;
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
        ignore (Board.move_tile board "w")
        (*1st move W is never valid on unshuffled board*);
        let final = Board.to_intlistlist board in
        let passed = initial = final in
        if not passed then
          board |> Board.to_intarrayarray |> intarrayarray_to_string
          |> print_endline;
        passed);
    QCheck2.Test.make ~count:8
      ~name:"A valid move on a new board takes it out of its initial state."
      ~print:QCheck.Print.(list int)
      QCheck2.Gen.(list_size (int_range 1 1) (int_range 2 9))
      (fun x ->
        let board = Board.initialize_board (List.nth x 0) in
        Board.fill_board board;
        let initial = Board.to_intlistlist board in
        ignore (Board.move_tile board "s")
        (*1st move S is always valid on unshuffled board*);
        let final = Board.to_intlistlist board in
        let passed = initial <> final in
        if not passed then
          board |> Board.to_intarrayarray |> intarrayarray_to_string
          |> print_endline;
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
        solve board moves;
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
        solve board moves;
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
          ignore (Board.move_tile board direction)
        done;
        solve init_board moves;
        let solved_board = Board.initialize_board size in
        Board.fill_board solved_board;
        Board.to_intarrayarray init_board = Board.to_intarrayarray solved_board);
    QCheck2.Test.make ~count:8 ~name:"Shuffling a board generates valid moves"
      (QCheck2.Gen.pair
         (QCheck2.Gen.int_range 3 10)
         (QCheck2.Gen.oneofl [ "easy"; "medium"; "hard" ]))
      (fun (size, difficulty) ->
        let board = Board.initialize_board size in
        Board.fill_board board;
        let board_copy = Board.copy_board board in
        let moves = Board.shuffle board difficulty in
        let moves_list =
          moves
          |> Stack.fold (fun acc x -> x :: acc) []
          |> List.filter (fun x -> x <> "")
        in
        List.fold_left
          (fun acc move ->
            if not acc then false
            else if not (Board.is_move_valid board_copy move) then false
            else (
              ignore (Board.move_tile board_copy move);
              true))
          true moves_list);
    QCheck2.Test.make ~count:15
      ~name:
        "Making any number of valid moves and then undoing them returns the \n\
         board to its initial state"
      ~print:QCheck.Print.(list int)
      QCheck2.Gen.(list_size (int_range 2 2) (int_range 2 9))
      (fun x ->
        let board = Board.initialize_board (List.nth x 0) in
        Board.fill_board board;
        let initial = Board.to_intlistlist board in
        let test_moves = Stack.create () in
        for _ = 1 to List.nth x 1 do
          let direction = List.nth [ "w"; "a"; "s"; "d" ] (Random.int 4) in
          if Board.is_move_valid board direction then (
            ignore (Board.move_tile board direction);
            Stack.push direction test_moves)
        done;
        for _ = 1 to List.nth x 1 do
          ignore (Board.undo board test_moves)
        done;
        let final = Board.to_intlistlist board in
        let passed = initial = final in
        if not passed then
          board |> Board.to_intarrayarray |> intarrayarray_to_string
          |> print_endline;
        passed);
    QCheck2.Test.make ~count:15
      ~name:
        "Undoing the moves made by shuffling the board returns the board to its \n\
         initial state"
      ~print:QCheck.Print.(list int)
      QCheck2.Gen.(list_size (int_range 2 2) (int_range 2 9))
      (fun x ->
        let board = Board.initialize_board (List.nth x 0) in
        Board.fill_board board;
        let initial = Board.to_intlistlist board in
        let shuf_moves = Board.shuffle board "easy" in
        for _ = 1 to Stack.length shuf_moves do
          ignore (Board.undo board shuf_moves)
        done;
        let final = Board.to_intlistlist board in
        let passed = initial = final in
        if not passed then
          board |> Board.to_intarrayarray |> intarrayarray_to_string
          |> print_endline;
        passed);
    QCheck2.Test.make ~count:8
      ~name:
        "Making multiple valid moves and then undoing only one does NOT return \n\
         the board to its initial state"
      ~print:QCheck.Print.(list int)
      QCheck2.Gen.(list_size (int_range 2 2) (int_range 2 9))
      (fun x ->
        let board = Board.initialize_board (List.nth x 0) in
        Board.fill_board board;
        let initial = Board.to_intlistlist board in
        let test_moves = Stack.create () in
        ignore (Board.move_tile board "d");
        Stack.push "d" test_moves;
        ignore (Board.move_tile board "a");
        Stack.push "a" test_moves;
        ignore (Board.undo board test_moves);
        let final = Board.to_intlistlist board in
        let passed = initial <> final in
        if not passed then
          board |> Board.to_intarrayarray |> intarrayarray_to_string
          |> print_endline;
        passed);
  ]

let make_is_move_valid_invalid_move_test name input board expected =
  name >:: fun _ ->
  let actual = Board.is_move_valid board input in
  assert_equal expected actual ~msg:name

let invalid_input_tests =
  "is_move_valid tests for an invalid input"
  >::: [
         make_is_move_valid_invalid_move_test "Invalid move input 'X'" "X"
           (Board.of_intarrayarray
              [| [| 1; 2; 3 |]; [| 4; 5; 6 |]; [| 7; 8; -1 |] |])
           false;
       ]

(*2048 tests*)
let qtests2 =
  [
    QCheck.Test.make ~name:"make_board contains exactly two 2s" QCheck.unit
      ~count:8 (fun () ->
        let board = Board2.make_board () in
        let int_list = Board2.to_intlistlist board in
        let twos_count = ref 0 in
        let valid = ref true in
        List.iter
          (fun row ->
            List.iter
              (fun value ->
                if value = -1 then ()
                else if value = 2 then incr twos_count
                else valid := false)
              row)
          int_list;
        (* Debugging output in case of failure *)
        if not (!twos_count = 2 || !valid) then (
          Printf.printf "Test failed. Board:\n";
          List.iter
            (fun row ->
              List.iter (fun value -> Printf.printf "%d " value) row;
              print_endline "")
            int_list;
          Printf.printf "Twos count: %d\n" !twos_count;
          Printf.printf "Valid: %b\n" !valid);
        !twos_count = 2 && !valid);
  ]

let make_turn_test name input expected =
  name >:: fun _ ->
  let grid = Board2.of_intarrayarray input in
  let actual = Board2.to_intarrayarray (Board2.turn grid) in
  assert_equal expected actual ~msg:name ~printer:intarrayarray_to_string

let turn_tests =
  "find empty tests"
  >::: [
         make_turn_test "one value in grid"
           [|
             [| -1; -1; -1; 2 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
           |]
           [|
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| 2; -1; -1; -1 |];
           |];
         make_turn_test "2 values turned"
           [|
             [| -1; 2; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; 2 |];
             [| -1; -1; -1; -1 |];
           |]
           [|
             [| -1; -1; -1; -1 |];
             [| 2; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; 2; -1 |];
           |];
         make_turn_test "doesn't change"
           [|
             [| -1; -1; -1; -1 |];
             [| -1; 2; 2; -1 |];
             [| -1; 2; 2; -1 |];
             [| -1; -1; -1; -1 |];
           |]
           [|
             [| -1; -1; -1; -1 |];
             [| -1; 2; 2; -1 |];
             [| -1; 2; 2; -1 |];
             [| -1; -1; -1; -1 |];
           |];
       ]

let make_compress_test name input expected =
  name >:: fun _ ->
  let grid = Board2.of_intarrayarray input in
  let actual = Board2.to_intarrayarray (Board2.compress grid) in
  assert_equal expected actual ~msg:name ~printer:intarrayarray_to_string

let compress_tests =
  "find empty tests"
  >::: [
         make_compress_test "one value in grid"
           [|
             [| -1; -1; -1; 2 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
           |]
           [|
             [| 2; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
           |];
         make_compress_test "2 values compressed, space in between"
           [|
             [| -1; 2; -1; 2 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
           |]
           [|
             [| 2; 2; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
           |];
         make_compress_test "middle values compressed"
           [|
             [| -1; -1; -1; -1 |];
             [| -1; 2; 2; -1 |];
             [| -1; 2; 2; -1 |];
             [| -1; -1; -1; -1 |];
           |]
           [|
             [| -1; -1; -1; -1 |];
             [| 2; 2; -1; -1 |];
             [| 2; 2; -1; -1 |];
             [| -1; -1; -1; -1 |];
           |];
       ]

let make_merge_test name input expected =
  name >:: fun _ ->
  let grid = Board2.of_intarrayarray input in
  let actual = Board2.to_intarrayarray (Board2.merge grid) in
  assert_equal expected actual ~msg:name ~printer:intarrayarray_to_string

let merge_tests =
  "merge rows"
  >::: [
         make_merge_test "one value in grid"
           [|
             [| 2; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
           |]
           [|
             [| 2; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
           |];
         make_merge_test "2 values compressed"
           [|
             [| 2; 2; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
           |]
           [|
             [| 4; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
           |];
         make_merge_test "full row"
           [|
             [| -1; -1; -1; -1 |];
             [| 2; 2; 2; 2 |];
             [| 2; 2; 2; 2 |];
             [| -1; -1; -1; -1 |];
           |]
           [|
             [| -1; -1; -1; -1 |];
             [| 4; -1; 4; -1 |];
             [| 4; -1; 4; -1 |];
             [| -1; -1; -1; -1 |];
           |];
       ]

let make_reverse_test name input expected =
  name >:: fun _ ->
  let grid = Board2.of_intarrayarray input in
  let actual = Board2.to_intarrayarray (Board2.reverse grid) in
  assert_equal expected actual ~msg:name

let reverse_tests =
  "reverse rows"
  >::: [
         make_reverse_test "one value in grid"
           [|
             [| 2; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
           |]
           [|
             [| -1; -1; -1; 2 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
           |];
         make_reverse_test "2 values reversed"
           [|
             [| -1; -1; -1; -1 |];
             [| -1; 2; -1; 2 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
           |]
           [|
             [| -1; -1; -1; -1 |];
             [| 2; -1; 2; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
           |];
         make_reverse_test "full row"
           [|
             [| -1; -1; -1; -1 |];
             [| 2; 2; 2; 2 |];
             [| 2; 2; 2; 2 |];
             [| -1; -1; -1; -1 |];
           |]
           [|
             [| -1; -1; -1; -1 |];
             [| 2; 2; 2; 2 |];
             [| 2; 2; 2; 2 |];
             [| -1; -1; -1; -1 |];
           |];
       ]

let move_left_test name input expected =
  name >:: fun _ ->
  let grid = Board2.of_intarrayarray input in
  let actual = Board2.to_intarrayarray (Board2.move_left grid) in
  assert_equal expected actual ~msg:name ~printer:intarrayarray_to_string

let left_tests =
  "move left"
  >::: [
         move_left_test "one value in grid"
           [|
             [| -1; -1; -1; 2 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
           |]
           [|
             [| 2; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
           |];
         move_left_test "3 values moved left"
           [|
             [| 2; 2; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; 2 |];
             [| -1; -1; -1; -1 |];
           |]
           [|
             [| 4; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| 2; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
           |];
         move_left_test "board filled"
           [|
             [| 2; 2; 2; 2 |];
             [| 2; 2; 2; 2 |];
             [| 2; 2; 2; 2 |];
             [| 2; 2; 2; 2 |];
           |]
           [|
             [| 4; 4; -1; -1 |];
             [| 4; 4; -1; -1 |];
             [| 4; 4; -1; -1 |];
             [| 4; 4; -1; -1 |];
           |];
       ]

let move_up_test name input expected =
  name >:: fun _ ->
  let grid = Board2.of_intarrayarray input in
  let actual = Board2.to_intarrayarray (Board2.move_up grid) in
  assert_equal expected actual ~msg:name ~printer:intarrayarray_to_string

let up_tests =
  "move up"
  >::: [
         move_up_test "one value in grid"
           [|
             [| -1; -1; -1; 2 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
           |]
           [|
             [| -1; -1; -1; 2 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
           |];
         move_up_test "3 values moved up"
           [|
             [| 2; 2; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| 2; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
           |]
           [|
             [| 4; 2; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
           |];
         move_up_test "board filled"
           [|
             [| 2; 2; 2; 2 |];
             [| 2; 2; 2; 2 |];
             [| 2; 2; 2; 2 |];
             [| 2; 2; 2; 2 |];
           |]
           [|
             [| 4; 4; 4; 4 |];
             [| 4; 4; 4; 4 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
           |];
       ]

let move_right_test name input expected =
  name >:: fun _ ->
  let grid = Board2.of_intarrayarray input in
  let actual = Board2.to_intarrayarray (Board2.move_right grid) in
  assert_equal expected actual ~msg:name ~printer:intarrayarray_to_string

let right_tests =
  "move right"
  >::: [
         move_right_test "one value in grid"
           [|
             [| 2; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
           |]
           [|
             [| -1; -1; -1; 2 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
           |];
         move_right_test "3 values moved right"
           [|
             [| 2; 2; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| 2; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
           |]
           [|
             [| -1; -1; -1; 4 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; 2 |];
             [| -1; -1; -1; -1 |];
           |];
         move_right_test "board filled"
           [|
             [| 2; 2; 2; 2 |];
             [| 2; 2; 2; 2 |];
             [| 2; 2; 2; 2 |];
             [| 2; 2; 2; 2 |];
           |]
           [|
             [| -1; -1; 4; 4 |];
             [| -1; -1; 4; 4 |];
             [| -1; -1; 4; 4 |];
             [| -1; -1; 4; 4 |];
           |];
       ]

let move_down_test name input expected =
  name >:: fun _ ->
  let grid = Board2.of_intarrayarray input in
  let actual = Board2.to_intarrayarray (Board2.move_down grid) in
  assert_equal expected actual ~msg:name ~printer:intarrayarray_to_string

let down_tests =
  "move down"
  >::: [
         move_down_test "one value in grid"
           [|
             [| 2; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
           |]
           [|
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| 2; -1; -1; -1 |];
           |];
         move_down_test "3 values moved down"
           [|
             [| 2; 2; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| 2; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
           |]
           [|
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| 4; 2; -1; -1 |];
           |];
         move_down_test "board filled"
           [|
             [| 2; 2; 2; 2 |];
             [| 2; 2; 2; 2 |];
             [| 2; 2; 2; 2 |];
             [| 2; 2; 2; 2 |];
           |]
           [|
             [| -1; -1; -1; -1 |];
             [| -1; -1; -1; -1 |];
             [| 4; 4; 4; 4 |];
             [| 4; 4; 4; 4 |];
           |];
       ]

let curr_state_test name input expected =
  name >:: fun _ ->
  let grid = Board2.of_intarrayarray input in
  let actual = Board2.curr_state grid in
  assert_equal expected actual ~msg:name

let curr_state_tests =
  "curr_state"
  >::: [
         curr_state_test "full grid with no potential moves"
           [|
             [| 2; 4; 8; 16 |];
             [| 4; 2; 4; 8 |];
             [| 8; 16; 2; 4 |];
             [| 16; 4; 8; 2 |];
           |]
           "LOST";
         curr_state_test "full grid with a potential moves"
           [|
             [| 2; 4; 8; 16 |];
             [| 4; 2; 2; 8 |];
             [| 8; 16; 2; 4 |];
             [| 16; 4; 8; 2 |];
           |]
           "GAME NOT OVER";
         curr_state_test "grid with empty tiles and no 2048 tile"
           [|
             [| 2; 4; 8; 16 |];
             [| 4; 2; 2; -1 |];
             [| 8; 16; 2; 4 |];
             [| 16; -1; 8; 2 |];
           |]
           "GAME NOT OVER";
         curr_state_test "2048 tile with empty tiles"
           [|
             [| 2; 2; 2; -1 |];
             [| 2; 2048; 2; 2 |];
             [| 2; -1; 2; 2 |];
             [| 2; 2; 2; -1 |];
           |]
           "WON";
         curr_state_test "2048 tile and full grid"
           [|
             [| 2; 2; 2; 2 |];
             [| 2; 2048; 2; 2 |];
             [| 2; 2; 2; 2 |];
             [| 2; 2; 2; 2 |];
           |]
           "WON";
       ]

let all_board_tests =
  "board tests"
  >::: [ tests; find_empty_tests; invalid_input_tests; basic_undo_tests ]

let all_board2_tests =
  "board2 tests"
  >::: [
         turn_tests;
         compress_tests;
         merge_tests;
         left_tests;
         up_tests;
         right_tests;
         down_tests;
         reverse_tests;
         curr_state_tests;
       ]

let all_board_tests =
  "board tests"
  >::: [ tests; find_empty_tests; invalid_input_tests; basic_undo_tests ]

let _ =
  Random.self_init ();
<<<<<<< Updated upstream
  run_test_tt_main all_board_tests;
=======
  run_test_tt_main tests;
  run_test_tt_main find_empty_tests;
  run_test_tt_main invalid_input_tests;
  run_test_tt_main basic_undo_tests;
>>>>>>> Stashed changes
  run_test_tt_main all_board2_tests;
  run_test_tt_main
    ("Slider Qcheck" >::: QCheck_runner.to_ounit2_test_list qtests);
  run_test_tt_main ("2048 Qcheck" >::: QCheck_runner.to_ounit2_test_list qtests2)
