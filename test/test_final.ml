open OUnit2
open Final

let tests = []

let qtests =
  [
    QCheck2.Test.make ~count:50
      ~name:
        "An filled board of size>1 must have one empty tile and one tile whose \
         value is size^2-1."
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
  ]

let _ =
  Random.self_init ();
  run_test_tt_main
    ("tests" >::: tests @ QCheck_runner.to_ounit2_test_list qtests)
