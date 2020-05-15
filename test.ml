open OUnit
open Static
open State

(** Test plan:
    - This test suite is designed to test the main functions of this system.
    - We have focused mainly on testing the validity of moves and jumps on the
      checkerboard to ensure a soundproof system.
    - For this test suite, we followed a glass-box testing approach. How the
      functions are tested are known to the user (they are all listed in the
      .mli files).
    - Our testing demonstrates correctness of the system because we have tested
      functions that are essential to the core of our system (valid moves/jumps, 
      checking pieces, anticipating AI, parsing commands) which the rest of the
      system relies on. *)

(** Helper function for cmpr_brds_row to compare inidividual pieces *)
let cmpr_element f_brd copy_brd row col acc = 
  let og_pc = f_brd.(row).(col) in
  let cp_pc = copy_brd.(row).(col) in
  let t_acc = if og_pc = cp_pc then true else false in
  t_acc

(** Helper function for cmpr_brs to iterate on columns gives a current row *)
let rec cmpr_brds_row f_brd copy_brd row cols acc = 
  match cols with 
  | 0 -> cmpr_element f_brd copy_brd row 0 acc
  | c -> cmpr_brds_row f_brd copy_brd row (c-1) (cmpr_element f_brd copy_brd row c acc)

(** Helper function for test_copy_function to iterate on rows. *)
let rec cmpr_brds f_brd copy_brd rows cols acc =
  match rows with 
  | 0 -> cmpr_brds_row f_brd copy_brd 0 cols acc
  | r -> cmpr_brds f_brd copy_brd (r-1) cols (cmpr_brds_row f_brd copy_brd 0 cols acc) 

(** Test 1 *)
(** function to test if the Static.copy function works 
    i.e if it correctly copies all elements in the correct locations *)
let test_copy_func = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  let copy_brd = State.make_copy f_brd in
  cmpr_brds f_brd copy_brd 7 7 false

(** Test 2 *)
(** Function to check if the remove function in state is working properly or not. 
    Returns a bool value*)
let test_remove_piece = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  State.remove_piece f_brd 7 0;
  let p = f_brd.(7).(0) in
  match p with
  | Nil -> true
  | _ -> false 

(** Test 3 *)
(** Function to check if a piece is a nil piece or not *)
let test_check_piece_nil = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  let get_p = State.check_piece f_brd 4 2 in
  let b_val = if get_p = "Nil" then true else false in
  b_val

(** Test 4 *)
(** Function to check if a piece is a king black piece or not *)
let test_check_piece_k_black = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  State.remove_piece f_brd 7 2;
  f_brd.(7).(2) <- King (Black,Some(7,2),10);
  let get_p = State.check_piece f_brd 7 2 in
  if get_p = "black-king" then true else false

(** Test 5 *)
(** Function to check if a piece is a king white piece or not *)
let test_check_piece_k_white = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  State.remove_piece f_brd 0 5;
  f_brd.(0).(5) <- King(White,Some(0,5),10);
  let get_p = State.check_piece f_brd 0 5 in
  if get_p = "white-king" then true else false

(** Test 6 *)
(** Function to check if a piece is a normal black piece or not *)
let test_check_piece_n_black = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  let get_p = State.check_piece f_brd 1 2 in
  let b_val = if get_p = "black-normal" then true else false in
  b_val

(** Test 7 *)
(** Function to check if a piece is a normal white piece or not *)
let test_check_piece_n_white = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  let get_p = State.check_piece f_brd 6 1 in
  let b_val = if get_p = "white-normal" then true else false in
  b_val

(** Test 8 *)
(** Test to check if the can_jump_function is working properly or not *)
let test_can_jump_to = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  State.can_jump_to f_brd 3 4 

(** Test 9 *)
(** Test to check is valid_jump is working properly or not for a valid jump of a 
    black normal piece over a white one *)
let test_valid_jump_invalid = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  f_brd.(3).(4) <- f_brd.(5).(4);
  f_brd.(5).(4) <- Nil;
  match State.valid_jump f_brd 2 3 4 5 with 
  | State _ -> true
  | _ -> false

(** Test 10 *)
(** Test to check is valid_jump is working properly or not for a valid jump of a 
    white normal piece over a black one *)
let test_valid_jump1 = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  f_brd.(4).(5) <- f_brd.(2).(3);
  f_brd.(2).(3) <- Nil;
  match State.valid_jump f_brd 5 6 3 4 with 
  | State _ -> true
  | _ -> false

(** Test 11 *)
(** Test to check is valid_jump is working properly or not for a valid jump of a 
    black king piece over a white one *)
let test_valid_jump2 = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  f_brd.(4).(5) <- King(Black,Some(4,5),10);
  State.remove_piece f_brd 6 3;
  match State.valid_jump f_brd 4 5 6 3 with 
  | State _ -> true
  | _ -> false

(** Test 12 *)
(** Test to check is valid_jump is working properly or not for a valid jump of a 
    white king piece over a black one *)
let test_valid_jump3 = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  State.remove_piece f_brd 1 2;
  f_brd.(1).(2) <- King(White,Some(1,2),10);
  match State.valid_jump f_brd 1 2 3 0 with 
  | State _ -> true
  | _ -> false

(** Test 13 *)
(** Test to check if is_jump works *)
let test_is_jump_t1 = 
  State.is_jump 4 6 2 4 

(** Test 14 *)
(** Test to check if is_jump works *)
let test_is_jump_t2 = 
  State.is_jump 2 4 4 6 

(** Test 15 *)
(** Test to check if is_jump works *)
let test_is_jump_t3 = 
  State.is_jump 4 3 6 5

(** Test 16 *)
(** Test to check if is_jump works *)
let test_is_jump_t4 = 
  State.is_jump 1 6 3 4

(** Test 17 *)
(** Test to check if move_standard works *)
let test_move_standard = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  State.move_standard f_brd 5 0 4 1;
  if State.check_piece f_brd 4 1 = "white-normal" then true else false

(** Test 18 *)
(** Test to check if valid move works (illegal move) *)
let test_valid_move_1 = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  match State.valid_move f_brd 8 8 8 8 with 
  | Illegal -> true 
  | _ -> false

(** Test 19 *)
(** Test to check if valid move works (illegal move) *)
let test_valid_move_2 = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  match State.valid_move f_brd 4 2 5 3 with 
  | Illegal -> true 
  | _ -> false

(** Test 20 *)
let test_valid_move_3 = 
  (** Test to check if valid move works (valid move) *)
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  match State.valid_move f_brd 2 1 3 2 with 
  | State _ -> true 
  | _ -> false

(** Test 21 *)
(** Test to check if valid move works (valid move) *)
let test_valid_move_4 = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  f_brd.(3).(4) <- f_brd.(5).(4);
  f_brd.(5).(4) <- Nil;
  match State.valid_move f_brd 2 3 4 5 with 
  | State _ -> true 
  | _ -> false

(** Test 22 *)
(** Test to check if valid move works (illegal move) *)
let test_valid_move_5 = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  f_brd.(3).(4) <- f_brd.(5).(4);
  f_brd.(5).(4) <- Nil;
  match State.valid_move f_brd 2 3 6 5 with 
  | Illegal -> true 
  | _ -> false

(** Test 23 *)
(** Test to check if valid move works (valid move) *)
let test_valid_move_6 = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  match State.valid_move f_brd 5 2 4 1 with 
  | State _ -> true 
  | _ -> false

(** Test 24 *)
(** Test to check if valid move works (valid move) *)
let test_valid_move_7 = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  f_brd.(4).(3) <- f_brd.(2).(3);
  f_brd.(2).(3) <- Nil;
  match State.valid_move f_brd 5 4 3 2 with 
  | State _ -> true 
  | _ -> false

(** Test 25 *)
(** Test to check if valid move works (illegal move) *)
let test_valid_move_8 = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  f_brd.(4).(3) <- f_brd.(2).(3);
  f_brd.(2).(3) <- Nil;
  match State.valid_move f_brd 5 4 1 2 with 
  | Illegal -> true 
  | _ -> false

(** Test 26 *)
(** Test to check if valid move works (illegal move) *)
let test_valid_move_9 = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  match State.valid_move f_brd 4 1 3 2 with 
  | Illegal -> true 
  | _ -> false

(** Test 27 *)
(** Test to check if valid move works (valid move) *)
let test_valid_move_10 = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  f_brd.(3).(4) <- King (Black,Some(3,4),10);
  match State.valid_move f_brd 3 4 4 3 with 
  | State _ -> true 
  | _ -> false

(** Test 28 *)
(** Test to check if valid move works (valid move) *)
let test_valid_move_11 = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  f_brd.(4).(1) <- King (White,Some(4,1),10);
  (* f_brd.(2).(3) <- Nil; *)
  match State.valid_move f_brd 4 1 3 2 with 
  | State _ -> true 
  | _ -> false

(** Test 29 *)
(** Test to check if valid move works (valid move) *)
let test_valid_move_12 = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  f_brd.(3).(4) <- King (Black,Some(3,4),10);
  f_brd.(2).(5) <- Nil;
  match State.valid_move f_brd 3 4 2 5 with 
  | State _ -> true
  | _ -> false

(** Test 30 *)
(** Test to check if valid move works (valid move) *)
let test_valid_move_13 = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  f_brd.(4).(1) <- King (White,Some(4,1),10);
  f_brd.(3).(2) <- Nil;
  match State.valid_move f_brd 4 1 3 2 with 
  | State _ -> true 
  | _ -> false

(** Test 31 *)
(** Test to check if valid move works (illegal move) *)
let test_valid_move_14 = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  f_brd.(4).(1) <- King (White,Some(4,1),10);
  f_brd.(2).(3) <- Nil;
  match State.valid_move f_brd 4 1 1 3 with 
  | Illegal -> true
  | _ -> false

(** Test 32 *)
(* Test to see that a white piece can't jump over another white piece *)
let test_valid_move_15 = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  f_brd.(4).(1) <- f_brd.(5).(0);
  match State.valid_move f_brd 5 2 3 0 with 
  | Illegal  -> true 
  | _ -> false

(** Test 33 *)
(** Test to see that a black piece can't jump over another black piece*)
let test_valid_move_16 = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  f_brd.(3).(4) <- f_brd.(2).(3);
  match State.valid_move f_brd 2 5 4 3 with 
  | Illegal  -> true 
  | _ -> false

(** Test 34 *)
(** Test to see that checking the piece works when inputting a piece *)
let test_check_piece_piece_1 = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  let p = f_brd.(2).(1) in
  if State.check_piece_piece p = "black-normal" then true else false

(** Test 35 *)
(** Test to see that checking the piece works when inputting a piece *)
let test_check_piece_piece_2 = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  let p = f_brd.(5).(0) in
  if State.check_piece_piece p = "white-normal" then true else false

(** Test 36 *)
(** Test to see that checking the piece works when inputting a piece *)
let test_check_piece_piece_3 = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  f_brd.(3).(0) <- King (White, Some(3,0), 10);
  let p = f_brd.(3).(0) in
  if State.check_piece_piece p = "white-king" then true else false

(** Test 37 *)
(** Test to see that checking the piece works when inputting a piece *)
let test_check_piece_piece_4 = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  f_brd.(3).(0) <- King (Black, Some(3,0), 10);
  let p = f_brd.(3).(0) in
  if State.check_piece_piece p = "black-king" then true else false

(** Test 38 *)
(** Test to see if command parsing works for quitting the game *)
let test_command_parse_1 = 
  match Command.parse "quit" with
  | Quit -> true
  | _ -> false

(** Test 39 *)
(** Test to see if command parsing works for moving a piece *)
let test_command_parse_2 = 
  match Command.parse "move 2 1 to 3 2" with
  | Move ((2,1), (3,2)) -> true
  | _ -> false

(** Test 40 *)
(** Test to see if command parsing works for easy AI *)
let test_command_parse_3 = 
  match Command.parse "eai" with
  | EasyAI -> true
  | _ -> false

(** Test 41 *)
(** Test to see if command parsing works for medium AI *)
let test_command_parse_4 = 
  match Command.parse "mai" with
  | MedAI -> true
  | _ -> false

(** Test 42 *)
(** Test to see if command parsing works for hard AI *)
let test_command_parse_5 = 
  match Command.parse "hai" with
  | HardAI -> true
  | _ -> false

(** Test 43 *)
(** Test to see if command parsing works for quitting the game *)
let test_command_parse_6 = 
  match Command.parse "quit    " with
  | Quit -> true
  | _ -> false

(** Test 44 *)
(** Test to see if check_lst_empty works for an empty node *)
let test_check_lst_empty_1 = 
  let n = Node ((10,(0,0),(0,0)),[]) in
  if State.check_lst_empty n = true then true else false

(** Test 45 *)
(** Test to see if check_lst_empty works for a non-empty node *)
let test_check_lst_empty_2 = 
  let n1 = Node ((10,(0,0),(0,0)),[]) in
  let n2 = Node ((10,(0,0),(0,0)),n1::[]) in
  if State.check_lst_empty n2 = false then true else false

(** Test 46 *)
(** Test to see that easy AI does the first possible move *)
let test_check_easyAI_1 = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  f_brd.(3).(0) <- f_brd.(2).(1);
  let comm = State.get_best_move_easy f_brd in
  if comm = "move 5 0 to 4 1" then true else false

(** Test 47 *)
(** Test to see that easy AI does the first possible move *)
let test_check_easyAI_2 = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  f_brd.(3).(2) <- f_brd.(2).(3);
  let comm = State.get_best_move_easy f_brd in
  if comm = "move 5 0 to 4 1" then true else false

(** Test 48 *)
(** Test to see that easy AI does the first possible move *)
let test_check_easyAI_3 = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  f_brd.(3).(4) <- f_brd.(2).(5);
  let comm = State.get_best_move_easy f_brd in
  if comm = "move 5 0 to 4 1" then true else false

(** Test 49 *)
(** Test to see that hard AI does the best move *)
let test_check_hardAI_1 = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  f_brd.(3).(2) <- f_brd.(2).(3);
  let comm = State.get_best_move f_brd in
  if comm = "move 5 6 to 4 5" then true else false

(** Test 50 *)
(** Test to see that hard AI does the best move *)
let test_check_hardAI_2 = 
  let f_brd = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 f_brd;
  f_brd.(3).(6) <- f_brd.(2).(7);
  let comm = State.get_best_move f_brd in
  if comm = "move 5 6 to 4 5" then true else false


let test1 = 
  [
    "Test 1" >:: (fun _ -> assert_equal(test_copy_func) (true));
    "Test 2" >:: (fun _ -> assert_equal(test_remove_piece) (true));
    "Test 3" >:: (fun _ -> assert_equal(test_check_piece_nil)(true));
    "Test 4" >:: (fun _ -> assert_equal(test_check_piece_k_black)(true));
    "Test 5" >:: (fun _ -> assert_equal(test_check_piece_k_white)(true));
    "Test 6" >:: (fun _ -> assert_equal(test_check_piece_n_black)(true));
    "Test 7" >:: (fun _ -> assert_equal(test_check_piece_n_white)(true));
    "Test 8" >:: (fun _ -> assert_equal(test_can_jump_to)(true));
    "Test 9" >:: (fun _ -> assert_equal(test_valid_jump_invalid)(true));
    "Test 10" >:: (fun _ -> assert_equal(test_valid_jump1)(true));
    "Test 11" >:: (fun _ -> assert_equal(test_valid_jump2)(true));
    "Test 12" >:: (fun _ -> assert_equal(test_valid_jump3)(true));
    "Test 13" >:: (fun _ -> assert_equal(test_is_jump_t1)(true));
    "Test 14" >:: (fun _ -> assert_equal(test_is_jump_t2)(true));
    "Test 15" >:: (fun _ -> assert_equal(test_is_jump_t3)(true));
    "Test 16" >:: (fun _ -> assert_equal(test_is_jump_t4)(true));
    "Test 17" >:: (fun _ -> assert_equal(test_move_standard)(true));
    "Test 18" >:: (fun _ -> assert_equal(test_valid_move_1)(true));
    "Test 19" >:: (fun _ -> assert_equal(test_valid_move_2)(true));
    "Test 20" >:: (fun _ -> assert_equal(test_valid_move_3)(true));
    "Test 21" >:: (fun _ -> assert_equal(test_valid_move_4)(true));
    "Test 22" >:: (fun _ -> assert_equal(test_valid_move_5)(true));
    "Test 23" >:: (fun _ -> assert_equal(test_valid_move_6)(true));
    "Test 24" >:: (fun _ -> assert_equal(test_valid_move_7)(true));
    "Test 25" >:: (fun _ -> assert_equal(test_valid_move_8)(true));
    "Test 26" >:: (fun _ -> assert_equal(test_valid_move_9)(true));
    "Test 27" >:: (fun _ -> assert_equal(test_valid_move_10)(true));
    "Test 28" >:: (fun _ -> assert_equal(test_valid_move_11)(true));
    "Test 29" >:: (fun _ -> assert_equal(test_valid_move_12)(true));
    "Test 30" >:: (fun _ -> assert_equal(test_valid_move_13)(true));
    "Test 31" >:: (fun _ -> assert_equal(test_valid_move_14)(true));
    "Test 32" >:: (fun _ -> assert_equal(test_valid_move_15)(true));
    "Test 33" >:: (fun _ -> assert_equal(test_valid_move_16)(true));
    "Test 34" >:: (fun _ -> assert_equal(test_check_piece_piece_1)(true));
    "Test 35" >:: (fun _ -> assert_equal(test_check_piece_piece_2)(true));
    "Test 36" >:: (fun _ -> assert_equal(test_check_piece_piece_3)(true));
    "Test 37" >:: (fun _ -> assert_equal(test_check_piece_piece_4)(true));
    "Test 38" >:: (fun _ -> assert_equal(test_command_parse_1)(true));
    "Test 39" >:: (fun _ -> assert_equal(test_command_parse_2)(true));
    "Test 40" >:: (fun _ -> assert_equal(test_command_parse_3)(true));
    "Test 41" >:: (fun _ -> assert_equal(test_command_parse_4)(true));
    "Test 42" >:: (fun _ -> assert_equal(test_command_parse_5)(true));
    "Test 43" >:: (fun _ -> assert_equal(test_command_parse_6)(true));
    "Test 44" >:: (fun _ -> assert_equal(test_check_lst_empty_1)(true));
    "Test 45" >:: (fun _ -> assert_equal(test_check_lst_empty_2)(true));
    "Test 46" >:: (fun _ -> assert_equal(test_check_easyAI_1)(true));
    "Test 47" >:: (fun _ -> assert_equal(test_check_easyAI_2)(true));
    "Test 48" >:: (fun _ -> assert_equal(test_check_easyAI_3)(true));
    "Test 49" >:: (fun _ -> assert_equal(test_check_hardAI_1)(true));
    "Test 50" >:: (fun _ -> assert_equal(test_check_hardAI_2)(true));
  ]


let tests = List.flatten[test1]

let suite = "search test suite" >::: tests

let _ = run_test_tt_main suite
