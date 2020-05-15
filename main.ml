open Unix

(** [color_cell n board] is the helper for placing a green check mark at all possible locations that a piece can
    be moved to. *)
let color_cell (n:((int*int) * (int*int))) (board: Static.piece array array): Static.piece array array = 
  let end_coords = n |> snd in
  board.(end_coords|>fst).(end_coords|>snd)<- Pos;
  board

(** [color_brd board lst] places a green check mark at all possible locations that a piece
    can be moved to *)
let rec color_brd (board: Static.piece array array)lst : Static.piece array array = 
  match lst with 
  | n::[] -> color_cell(n)(board)
  | h::t -> color_brd(color_cell h board)t

(** Normal Multiplayer *)
(** [play_game_black board str start_time] provides the interface for a normal untimed game for when 
    the player black's turn is up. *)
let rec play_game_black board str start_time = 
  let res = State.execute_command board str "black" start_time in
  match res with
  | State x ->
    (  ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
       print_endline "";
       print_string "    ";
       Static.print_line(40);
       print_endline "";
       Static.pp_arr 0 0 x;
       Stdlib.print_string "\n";
       Stdlib.print_string "It is player white's turn\n";
       if ((State.pos_moves_white x 7 7 []) = []) then (Stdlib.print_string "\n";
                                                        Stdlib.print_string "No more valid moves for white player. Red has won!\n"; Stdlib.exit 0) else
         Stdlib.print_string "Type in another command.\n";
       print_string "> ";
       let com_str = Stdlib.read_line(); in
       play_game_white x com_str start_time)
  | Quit_Game ->  ANSITerminal.(print_string[cyan] "Thank you for playing with us!");print_endline "";Stdlib.exit 0
  | Hint_Game -> (
      let lst = State.pos_moves_black board 7 7 [] in
      ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
      print_endline "";
      print_string "    ";
      Static.print_line(40);
      print_endline "";
      let t_brd = State.make_copy board in
      let n_brd = color_brd (t_brd) lst in
      Static.pp_arr 0 0 n_brd;
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_game_black board com_str start_time 
    )
  | Help_Game -> (
      print_endline "";
      ANSITerminal.(print_string [white]
                      "Let me give you a basic rundown of our game.
                  We have two game options: to play agaisnt another person or to play
                  against our computer. We also have the option of timed and untimed checkers.
                  Timed checkers will run the game for the specified amount of time and will end
                  as soon as the time is up. On the other hand, untimed will run till someone 
                  wins or typed quit.
                  - quit : typed when you want to exit the game
                  - hint : typed when you want to see all the possible moves you can make
                  - help : to show all this information all over again.
                  Have a fun game!");
      print_endline "";
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_game_black board com_str start_time 

    )
  | Illegal -> (
      Stdlib.print_string "\n";
      Stdlib.print_string "Illegal move, try again & type in a new command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_game_black board com_str start_time)

(** [play_game_white board str start_time] provides the interface for a normal untimed game for when 
    the player white's turn is up. *)
and play_game_white board str start_time = 
  let res = State.execute_command board str "white" start_time in
  match res with
  | State x ->
    (  ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
       print_endline "";
       print_string "    ";
       Static.print_line(40);
       print_endline "";
       Static.pp_arr 0 0 x;
       Stdlib.print_string "\n";
       Stdlib.print_string "It is player red's turn\n";
       if ((State.pos_moves_black x 7 7 []) = []) then (Stdlib.print_string "\n";
                                                        Stdlib.print_string "No more valid moves for red player. White has won!\n"; Stdlib.exit 0) else
         Stdlib.print_string "Type in another command.\n";
       print_string "> ";
       let com_str = Stdlib.read_line(); in
       play_game_black x com_str start_time)
  | Quit_Game ->  ANSITerminal.(print_string[cyan] "Thank you for playing with us!");print_endline "";Stdlib.exit 0
  | Hint_Game -> (
      let lst = State.pos_moves_white board 7 7 [] in
      ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
      print_endline "";
      print_string "    ";
      Static.print_line(40);
      print_endline "";
      let t_brd = State.make_copy board in
      let n_brd = color_brd (t_brd) lst in
      Static.pp_arr 0 0 n_brd;
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_game_white board com_str start_time 
    )
  | Help_Game -> (
      print_endline "";
      ANSITerminal.(print_string [white]
                      "Let me give you a basic rundown of our game.
                  We have two game options: to play agaisnt another person or to play
                  against our computer. We also have the option of timed and untimed checkers.
                  Timed checkers will run the game for the specified amount of time and will end
                  as soon as the time is up. On the other hand, untimed will run till someone 
                  wins or typed quit.
                  - quit : typed when you want to exit the game
                  - hint : typed when you want to see all the possible moves you can make
                  - help : to show all this information all over again.
                  Have a fun game!");
      print_endline "";
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_game_white board com_str start_time 

    )
  | Illegal -> (
      Stdlib.print_string "\n";
      Stdlib.print_string "Illegal move, try again & type in a new command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_game_white board com_str start_time)

(** [play_timed_game_white board str start_time length] provides the interface for a normal timed game of [length]
    seconds for when the player white's turn is up. *)
and play_timed_game_white board str start_time length = 
  let res = State.execute_command_time board str "white" start_time length in
  match res with
  | State x ->
    (  ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
       print_endline "";
       print_string "    ";
       Static.print_line(40);
       print_endline "";
       Static.pp_arr 0 0 x;
       Stdlib.print_string "\n";
       Stdlib.print_string "It is player red's turn\n";
       if ((State.pos_moves_black x 7 7 []) = []) then (Stdlib.print_string "\n";
                                                        Stdlib.print_string "No more valid moves for red player. White has won!\n"; Stdlib.exit 0) else
         Stdlib.print_string "Type in another command.\n";
       print_string "> ";
       let com_str = Stdlib.read_line(); in
       play_timed_game_white x com_str start_time length)
  | Quit_Game ->  ANSITerminal.(print_string[cyan] "Thank you for playing with us!");print_endline "";Stdlib.exit 0
  | Hint_Game -> (
      let lst = State.pos_moves_white board 7 7 [] in
      ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
      print_endline "";
      print_string "    ";
      Static.print_line(40);
      print_endline "";
      let t_brd = State.make_copy board in
      let n_brd = color_brd (t_brd) lst in
      Static.pp_arr 0 0 n_brd;
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_timed_game_white board com_str start_time length
    )
  | Help_Game -> (
      print_endline "";
      ANSITerminal.(print_string [white]
                      "Let me give you a basic rundown of our game.
                  We have two game options: to play agaisnt another person or to play
                  against our computer. We also have the option of timed and untimed checkers.
                  Timed checkers will run the game for the specified amount of time and will end
                  as soon as the time is up. On the other hand, untimed will run till someone 
                  wins or typed quit.
                  - quit : typed when you want to exit the game
                  - hint : typed when you want to see all the possible moves you can make
                  - help : to show all this information all over again.
                  Have a fun game!");
      print_endline "";
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_timed_game_white board com_str start_time length

    )
  | Illegal -> (
      Stdlib.print_string "\n";
      Stdlib.print_string "Illegal move, try again & type in a new command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_timed_game_white board com_str start_time length)

(** [play_timed_game_black board str start_time length] provides the interface for a normal timed game of [length]
    seconds for when the player black's turn is up. *)
and play_timed_game_black board str start_time length = 
  let res = State.execute_command_time board str "black" start_time length in
  match res with
  | State x ->
    (  ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
       print_endline "";
       print_string "    ";
       Static.print_line(40);
       print_endline "";
       Static.pp_arr 0 0 x;
       Stdlib.print_string "\n";
       Stdlib.print_string "It is player white's turn\n";
       if ((State.pos_moves_white x 7 7 []) = []) then (Stdlib.print_string "\n";
                                                        Stdlib.print_string "No more valid moves for white player. Red has won!\n"; Stdlib.exit 0) else
         Stdlib.print_string "Type in another command.\n";
       print_string "> ";
       let com_str = Stdlib.read_line(); in
       play_timed_game_white x com_str start_time length)
  | Quit_Game ->  ANSITerminal.(print_string[cyan] "Thank you for playing with us!");print_endline "";Stdlib.exit 0
  | Hint_Game -> (
      let lst = State.pos_moves_white board 7 7 [] in
      ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
      print_endline "";
      print_string "    ";
      Static.print_line(40);
      print_endline "";
      color_brd (State.make_copy board) lst;
      let t_brd = State.make_copy board in
      let n_brd = color_brd (t_brd) lst in
      Static.pp_arr 0 0 n_brd;
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_timed_game_black board com_str start_time length
    )
  | Help_Game -> (
      print_endline "";
      ANSITerminal.(print_string [white]
                      "Let me give you a basic rundown of our game.
                  We have two game options: to play agaisnt another person or to play
                  against our computer. We also have the option of timed and untimed checkers.
                  Timed checkers will run the game for the specified amount of time and will end
                  as soon as the time is up. On the other hand, untimed will run till someone 
                  wins or typed quit.
                  - quit : typed when you want to exit the game
                  - hint : typed when you want to see all the possible moves you can make
                  - help : to show all this information all over again.
                  Have a fun game!");
      print_endline "";
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_timed_game_black board com_str start_time length

    )
  | Illegal -> (
      Stdlib.print_string "\n";
      Stdlib.print_string "Illegal move, try again & type in a new command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_timed_game_black board com_str start_time length)



(** Timed - Medium AI *)
(** [play_timed_game_black_med_ai board str start_time length] provides the interface for a timed game of [length]
    seconds against the mdeium level AI for when the player black's turn is up. *)
let rec play_timed_game_black_med_ai board str start_time length = 
  let res = State.execute_command_time board str "black" start_time length in
  match res with
  | State x ->
    (  ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
       print_endline "";
       print_string "    ";
       Static.print_line(40);
       print_endline "";
       Static.pp_arr 0 0 x;
       Stdlib.print_string "\n";
       Stdlib.print_string "It is player white's turn\n";
       if ((State.pos_moves_white x 7 7 []) = []) then (Stdlib.print_string "\n";
                                                        Stdlib.print_string "No more valid moves for white player. Red has won!\n"; Stdlib.exit 0) else
         play_timed_game_white_med_ai x start_time length)
  | Quit_Game -> ANSITerminal.(print_string[cyan] "Thank you for playing with us!");print_endline "";Stdlib.exit 0
  | Hint_Game -> (
      let lst = State.pos_moves_black board 7 7 [] in
      ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
      print_endline "";
      print_string "    ";
      Static.print_line(40);
      print_endline "";
      let t_brd = State.make_copy board in
      let n_brd = color_brd (t_brd) lst in
      Static.pp_arr 0 0 n_brd;
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_timed_game_black_med_ai board com_str start_time length
    )
  | Help_Game -> (
      print_endline "";
      ANSITerminal.(print_string [white]
                      "Let me give you a basic rundown of our game.
                  We have two game options: to play agaisnt another person or to play
                  against our computer. We also have the option of timed and untimed checkers.
                  Timed checkers will run the game for the specified amount of time and will end
                  as soon as the time is up. On the other hand, untimed will run till someone 
                  wins or typed quit.
                  - quit : typed when you want to exit the game
                  - hint : typed when you want to see all the possible moves you can make
                  - help : to show all this information all over again.
                  Have a fun game!");
      print_endline "";
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_timed_game_black_med_ai board com_str start_time length

    )
  | Illegal -> (
      Stdlib.print_string "\n";
      Stdlib.print_string "Illegal move, try again & type in a new command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_timed_game_black_med_ai board com_str start_time length)

(** [play_timed_game_white_med_ai board start_time length] provides the interface for a timed game of [length]
    seconds against the mdeium level AI for when the player white's turn is up. *)
and play_timed_game_white_med_ai board (start_time:float) length = 
  let t_board = State.make_copy board in
  let str1 = State.get_best_move_med t_board in
  let res = State.execute_command_time board str1 "white" start_time length in
  match res with
  | State x ->
    (  ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
       print_endline "";
       print_string "    ";
       Static.print_line(40);
       print_endline "";
       Static.pp_arr 0 0 x;
       Stdlib.print_string "\n";
       Stdlib.print_string "It is player red's turn\n";
       if ((State.pos_moves_black x 7 7 []) = []) then (Stdlib.print_string "\n";
                                                        Stdlib.print_string "No more valid moves for red player. White has won!\n"; Stdlib.exit 0) else
         Stdlib.print_string "Type in another command.\n";
       print_string "> ";
       let com_str = Stdlib.read_line(); in
       play_timed_game_black_med_ai x com_str start_time length)
  | Quit_Game -> ANSITerminal.(print_string[cyan] "Thank you for playing with us!");print_endline "";Stdlib.exit 0
  | Hint_Game -> (
      let lst = State.pos_moves_black board 7 7 [] in
      ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
      print_endline "";
      print_string "    ";
      Static.print_line(40);
      print_endline "";
      let t_brd = State.make_copy board in
      let n_brd = color_brd (t_brd) lst in
      Static.pp_arr 0 0 n_brd;
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_timed_game_white_med_ai board start_time length
    )
  | Help_Game -> (
      print_endline "";
      ANSITerminal.(print_string [white]
                      "Let me give you a basic rundown of our game.
                  We have two game options: to play agaisnt another person or to play
                  against our computer. We also have the option of timed and untimed checkers.
                  Timed checkers will run the game for the specified amount of time and will end
                  as soon as the time is up. On the other hand, untimed will run till someone 
                  wins or typed quit.
                  - quit : typed when you want to exit the game
                  - hint : typed when you want to see all the possible moves you can make
                  - help : to show all this information all over again.
                  Have a fun game!");
      print_endline "";
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_timed_game_white_med_ai board start_time length

    )
  | Illegal -> (
      Stdlib.print_string "\n";
      Stdlib.print_string "Illegal move, try again & type in a new command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_timed_game_white_med_ai board start_time length)
(** End of timed-medium-Ai *)

(** Untimed - Medium AI *)
(** [play_game_black_med_ai board str start_time] provides the interface for an untimed game
    against the medium level AI for when the player black's turn is up. *)
let rec play_game_black_med_ai board str start_time = 
  let res = State.execute_command board str "black" start_time in
  match res with
  | State x ->
    (  ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
       print_endline "";
       print_string "    ";
       Static.print_line(40);
       print_endline "";
       Static.pp_arr 0 0 x;
       Stdlib.print_string "\n";
       Stdlib.print_string "It is player white's turn\n";
       if ((State.pos_moves_white x 7 7 []) = []) then (Stdlib.print_string "\n";
                                                        Stdlib.print_string "No more valid moves for white player. Red has won!\n"; Stdlib.exit 0) else
         (* Stdlib.print_string "Type in another command.\n";
            print_string "> ";
            let com_str = Stdlib.read_line(); in *)
         play_game_white_med_ai x start_time) 
  | Quit_Game -> ANSITerminal.(print_string[cyan] "Thank you for playing with us!");print_endline "";Stdlib.exit 0
  | Hint_Game -> (
      let lst = State.pos_moves_black board 7 7 [] in
      ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
      print_endline "";
      print_string "    ";
      Static.print_line(40);
      print_endline "";
      let t_brd = State.make_copy board in
      let n_brd = color_brd (t_brd) lst in
      Static.pp_arr 0 0 n_brd;
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_game_black_med_ai board com_str start_time 
    )
  | Help_Game -> (
      print_endline "";
      ANSITerminal.(print_string [white]
                      "Let me give you a basic rundown of our game.
                  We have two game options: to play agaisnt another person or to play
                  against our computer. We also have the option of timed and untimed checkers.
                  Timed checkers will run the game for the specified amount of time and will end
                  as soon as the time is up. On the other hand, untimed will run till someone 
                  wins or typed quit.
                  - quit : typed when you want to exit the game
                  - hint : typed when you want to see all the possible moves you can make
                  - help : to show all this information all over again.
                  Have a fun game!");
      print_endline "";
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_game_black_med_ai board com_str start_time 

    )
  | Illegal -> (
      Stdlib.print_string "\n";
      Stdlib.print_string "Illegal move, try again & type in a new command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_game_black_med_ai board com_str start_time)

(** [play_game_white_med_ai board start_time] provides the interface for an untimed game
    against the medium level AI for when the player white's turn is up. *)
and play_game_white_med_ai board start_time = 
  let t_board = State.make_copy board in
  let str1 = State.get_best_move_med t_board in
  let res = State.execute_command board str1 "white" start_time in
  match res with
  | State x ->
    (  ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
       print_endline "";
       print_string "    ";
       Static.print_line(40);
       print_endline "";
       Static.pp_arr 0 0 x;
       Stdlib.print_string "\n";
       Stdlib.print_string "It is player red's turn\n";
       if ((State.pos_moves_black x 7 7 []) = []) then (Stdlib.print_string "\n";
                                                        Stdlib.print_string "No more valid moves for red player. White has won!\n"; Stdlib.exit 0) else
         Stdlib.print_string "Type in another command.\n";
       print_string "> ";
       let com_str = Stdlib.read_line(); in
       play_game_black_med_ai x com_str start_time)
  | Quit_Game -> ANSITerminal.(print_string[cyan] "Thank you for playing with us!");print_endline "";Stdlib.exit 0
  | Hint_Game -> (
      let lst = State.pos_moves_black board 7 7 [] in
      ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
      print_endline "";
      print_string "    ";
      Static.print_line(40);
      print_endline "";
      let t_brd = State.make_copy board in
      let n_brd = color_brd (t_brd) lst in
      Static.pp_arr 0 0 n_brd;
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_game_white_med_ai board start_time 
    )
  | Help_Game -> (
      print_endline "";
      ANSITerminal.(print_string [white]
                      "Let me give you a basic rundown of our game.
                  We have two game options: to play agaisnt another person or to play
                  against our computer. We also have the option of timed and untimed checkers.
                  Timed checkers will run the game for the specified amount of time and will end
                  as soon as the time is up. On the other hand, untimed will run till someone 
                  wins or typed quit.
                  - quit : typed when you want to exit the game
                  - hint : typed when you want to see all the possible moves you can make
                  - help : to show all this information all over again.
                  Have a fun game!");
      print_endline "";
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_game_white_med_ai board start_time 

    )
  | Illegal -> (
      Stdlib.print_string "\n";
      Stdlib.print_string "Illegal move, try again & type in a new command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_game_white_med_ai board start_time)

(** End of Untimed med- AI *)

(** [how_long_med length] provides the interface after the user is asked to enter the duration 
    of the game against the medium level AI *)
let how_long_med length = 
  let init_board = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 init_board;
  ANSITerminal.(print_string[green] "    |  0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
  print_endline "";
  print_string "    ";
  Static.print_line(40);
  print_endline "";
  Static.pp_arr 0 0 init_board;
  print_endline "It's player red's turn. \n";
  let start_time = Unix.time() in 
  print_endline "Type in a command to begin";
  print_string "> ";
  match read_line () with
  | command_str -> play_timed_game_black_med_ai init_board command_str start_time length

(** [timed_med] provides the interface prompting the user to enter the desired duration of 
    the game against a medium level AI *)
let timed_med () = 
  ANSITerminal.(print_string [yellow]
                  "Type the length of the game in seconds.");
  print_endline "";
  print_string ">";
  let opt = Stdlib.read_line () in
  how_long_med (float_of_string opt)


(** [untimed_med] provides the interface after the user selects an untimed game against a 
    medium level AI *)
let untimed_med () = 
  let init_board = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 init_board;
  ANSITerminal.(print_string[green] "    |  0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
  print_endline "";
  print_string "    ";
  Static.print_line(40);
  print_endline "";
  Static.pp_arr 0 0 init_board;
  print_endline "It's player red's turn. \n";
  let start_time = Unix.time() in 
  print_endline "Type in a command to begin";
  print_string "> ";
  match read_line () with
  | command_str -> play_game_black_med_ai init_board command_str start_time



(* let hard_ai () = 
   ANSITerminal.(print_string [cyan]
                  "\nWelcome to the hardest game against our AI.\n");
   print_endline "";
   let init_board = Static.create_board 8 8 in
   Static.create_board_init_h 8 8 init_board;
   ANSITerminal.(print_string[green] "    |  0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
   print_endline "";
   print_string "    ";
   Static.print_line(40);
   print_endline "";
   Static.pp_arr 0 0 init_board;
   print_endline "It's player red's turn. \n";
   let start_time = Unix.time() in 
   print_endline "Type in a command to begin";
   print_string "> ";
   match read_line () with
   | command_str -> play_game_black_hard_ai init_board command_str start_time *)

(** [medium_ai]  *)
(** [medium_ai] provides the interface after the use has selected a game against a medium
    level AI *)
let rec medium_ai () = 
  ANSITerminal.(print_string [cyan]
                  "\nWelcome to the medium game against our AI.\n");
  print_endline "";
  ANSITerminal.(print_string [yellow]
                  "\n\nChoose timed or untimed");
  print_endline "";
  ANSITerminal.(print_string [yellow]
                  "Type timed for a timed game and untimed for an untimed game");
  print_endline "";
  print_string ">";
  let opt = Stdlib.read_line () in
  match opt with 
  | "timed" -> timed_med ()
  | "untimed" -> untimed_med () 
  | _ -> print_endline "Incorrect input. Try again!"; medium_ai()  

(** Timed - Easy AI *)
(** [play_timed_game_black_easy_ai board str start_time length] provides the interface
    for the user against an easy level AI in a timed game of [length] seconds for when it is player black's turn *)
let rec play_timed_game_black_easy_ai board str start_time length = 
  let res = State.execute_command_time board str "black" start_time length in
  match res with
  | State x ->
    (  ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
       print_endline "";
       print_string "    ";
       Static.print_line(40);
       print_endline "";
       Static.pp_arr 0 0 x;
       Stdlib.print_string "\n";
       Stdlib.print_string "It is player white's turn\n";
       if ((State.pos_moves_white x 7 7 []) = []) then (Stdlib.print_string "\n";
                                                        Stdlib.print_string "No more valid moves for white player. Red has won!\n"; Stdlib.exit 0) else
         play_timed_game_white_easy_ai x start_time length)
  | Quit_Game -> ANSITerminal.(print_string[cyan] "Thank you for playing with us!");print_endline "";Stdlib.exit 0
  | Hint_Game -> (
      let lst = State.pos_moves_black board 7 7 [] in
      ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
      print_endline "";
      print_string "    ";
      Static.print_line(40);
      print_endline "";
      let t_brd = State.make_copy board in
      let n_brd = color_brd (t_brd) lst in
      Static.pp_arr 0 0 n_brd;
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_timed_game_black_easy_ai board com_str start_time length
    )
  | Help_Game -> (
      print_endline "";
      ANSITerminal.(print_string [white]
                      "Let me give you a basic rundown of our game.
                  We have two game options: to play agaisnt another person or to play
                  against our computer. We also have the option of timed and untimed checkers.
                  Timed checkers will run the game for the specified amount of time and will end
                  as soon as the time is up. On the other hand, untimed will run till someone 
                  wins or typed quit.
                  - quit : typed when you want to exit the game
                  - hint : typed when you want to see all the possible moves you can make
                  - help : to show all this information all over again.
                  Have a fun game!");
      print_endline "";
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_timed_game_black_easy_ai board com_str start_time length

    )
  | Illegal -> (
      Stdlib.print_string "\n";
      Stdlib.print_string "Illegal move, try again & type in a new command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_timed_game_black_easy_ai board com_str start_time length)

(** [play_timed_game_white_easy_ai board str start_time length] provides the interface
    for the user against an easy level AI in a timed game of [length] seconds for when it is player white's turn *)
and play_timed_game_white_easy_ai board (start_time:float) length = 
  let t_board = State.make_copy board in
  let str1 = State.get_best_move_easy t_board in
  let res = State.execute_command_time board str1 "white" start_time length in
  match res with
  | State x ->
    (  ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
       print_endline "";
       print_string "    ";
       Static.print_line(40);
       print_endline "";
       Static.pp_arr 0 0 x;
       Stdlib.print_string "\n";
       Stdlib.print_string "It is player red's turn\n";
       if ((State.pos_moves_black x 7 7 []) = []) then (Stdlib.print_string "\n";
                                                        Stdlib.print_string "No more valid moves for red player. White has won!\n"; Stdlib.exit 0) else
         Stdlib.print_string "Type in another command.\n";
       print_string "> ";
       let com_str = Stdlib.read_line(); in
       play_timed_game_black_easy_ai x com_str start_time length)
  | Quit_Game -> ANSITerminal.(print_string[cyan] "Thank you for playing with us!");print_endline "";Stdlib.exit 0
  | Hint_Game -> (
      let lst = State.pos_moves_black board 7 7 [] in
      ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
      print_endline "";
      print_string "    ";
      Static.print_line(40);
      print_endline "";
      let t_brd = State.make_copy board in
      let n_brd = color_brd (t_brd) lst in
      Static.pp_arr 0 0 n_brd;
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_timed_game_white_easy_ai board start_time length
    )
  | Help_Game -> (
      print_endline "";
      ANSITerminal.(print_string [white]
                      "Let me give you a basic rundown of our game.
                  We have two game options: to play agaisnt another person or to play
                  against our computer. We also have the option of timed and untimed checkers.
                  Timed checkers will run the game for the specified amount of time and will end
                  as soon as the time is up. On the other hand, untimed will run till someone 
                  wins or typed quit.
                  - quit : typed when you want to exit the game
                  - hint : typed when you want to see all the possible moves you can make
                  - help : to show all this information all over again.
                  Have a fun game!");
      print_endline "";
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_timed_game_white_easy_ai board start_time length

    )
  | Illegal -> (
      Stdlib.print_string "\n";
      Stdlib.print_string "Illegal move, try again & type in a new command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_timed_game_white_easy_ai board start_time length)
(** End of timed-medium-Ai *)

(** Untimed - Medium AI *)
(** [play__game_black_easy_ai board str start_time] provides the interface
    for the user against an easy level AI in an timed game for when it is player black's turn *)
let rec play_game_black_easy_ai board str start_time = 
  let res = State.execute_command board str "black" start_time in
  match res with
  | State x ->
    (  ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
       print_endline "";
       print_string "    ";
       Static.print_line(40);
       print_endline "";
       Static.pp_arr 0 0 x;
       Stdlib.print_string "\n";
       if ((State.pos_moves_white x 7 7 []) = []) then (Stdlib.print_string "\n";
                                                        Stdlib.print_string "No more valid moves for white player. Red has won!\n"; Stdlib.exit 0) else
         (* Stdlib.print_string "Type in another command.\n";
            print_string "> ";
            let com_str = Stdlib.read_line(); in *)
         play_game_white_easy_ai x start_time) 
  | Quit_Game -> ANSITerminal.(print_string[cyan] "Thank you for playing with us!");print_endline "";Stdlib.exit 0
  | Hint_Game -> (
      let lst = State.pos_moves_black board 7 7 [] in
      ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
      print_endline "";
      print_string "    ";
      Static.print_line(40);
      print_endline "";
      let t_brd = State.make_copy board in
      let n_brd = color_brd (t_brd) lst in
      Static.pp_arr 0 0 n_brd;
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_game_black_easy_ai board com_str start_time 
    )
  | Help_Game -> (
      print_endline "";
      ANSITerminal.(print_string [white]
                      "Let me give you a basic rundown of our game.
                  We have two game options: to play agaisnt another person or to play
                  against our computer. We also have the option of timed and untimed checkers.
                  Timed checkers will run the game for the specified amount of time and will end
                  as soon as the time is up. On the other hand, untimed will run till someone 
                  wins or typed quit.
                  - quit : typed when you want to exit the game
                  - hint : typed when you want to see all the possible moves you can make
                  - help : to show all this information all over again.
                  Have a fun game!");
      print_endline "";
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_game_black_easy_ai board com_str start_time 
    )
  | Illegal -> (
      Stdlib.print_string "\n";
      Stdlib.print_string "Illegal move, try again & type in a new command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_game_black_easy_ai board com_str start_time)

(** [play__game_white_easy_ai board str start_time] provides the interface
    for the user against an easy level AI in an timed game for when it is player white's turn *)
and play_game_white_easy_ai board start_time = 
  let t_board = State.make_copy board in
  let str1 = State.get_best_move_easy t_board in
  let res = State.execute_command board str1 "white" start_time in
  match res with
  | State x ->
    (  ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
       print_endline "";
       print_string "    ";
       Static.print_line(40);
       print_endline "";
       Static.pp_arr 0 0 x;
       Stdlib.print_string "\n";
       Stdlib.print_string "It is player red's turn\n";
       if ((State.pos_moves_black x 7 7 []) = []) then (Stdlib.print_string "\n";
                                                        Stdlib.print_string "No more valid moves for red player. White has won!\n"; Stdlib.exit 0) else
         Stdlib.print_string "Type in another command.\n";
       print_string "> ";
       let com_str = Stdlib.read_line(); in
       play_game_black_easy_ai x com_str start_time)
  | Quit_Game -> ANSITerminal.(print_string[cyan] "Thank you for playing with us!");print_endline "";Stdlib.exit 0
  | Hint_Game -> (
      let lst = State.pos_moves_black board 7 7 [] in
      ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
      print_endline "";
      print_string "    ";
      Static.print_line(40);
      print_endline "";
      let t_brd = State.make_copy board in
      let n_brd = color_brd (t_brd) lst in
      Static.pp_arr 0 0 n_brd;
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_game_white_easy_ai board start_time 
    )
  | Help_Game -> (
      print_endline "";
      ANSITerminal.(print_string [white]
                      "Let me give you a basic rundown of our game.
                  We have two game options: to play agaisnt another person or to play
                  against our computer. We also have the option of timed and untimed checkers.
                  Timed checkers will run the game for the specified amount of time and will end
                  as soon as the time is up. On the other hand, untimed will run till someone 
                  wins or typed quit.
                  - quit : typed when you want to exit the game
                  - hint : typed when you want to see all the possible moves you can make
                  - help : to show all this information all over again.
                  Have a fun game!");
      print_endline "";
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_game_white_easy_ai board start_time 
    )
  | Illegal -> (
      Stdlib.print_string "\n";
      Stdlib.print_string "Illegal move, try again & type in a new command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_game_white_easy_ai board start_time)

(** End of Untimed med- AI *)

(** [how_long_easy length] provides the interface for a game after the user has chosen a timed game of 
    [length] seconds against an easy AI *)
let how_long_easy length = 
  let init_board = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 init_board;
  ANSITerminal.(print_string[green] "    |  0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
  print_endline "";
  print_string "    ";
  Static.print_line(40);
  print_endline "";
  Static.pp_arr 0 0 init_board;
  print_endline "It's player red's turn. \n";
  let start_time = Unix.time() in 
  print_endline "Type in a command to begin";
  print_string "> ";
  match read_line () with
  | command_str -> play_timed_game_black_easy_ai init_board command_str start_time length

(** [timed_easy] provides the interface asking the user how long they want to play for 
    after they have chosen a timed against an easy AI *)
let timed_easy () = 
  ANSITerminal.(print_string [yellow]
                  "Type the length of the game in seconds.");
  print_endline "";
  print_string ">";
  let opt = Stdlib.read_line () in
  how_long_easy (float_of_string opt)

(** [untimed_easy] provides the interface after a user has selected an untimed game 
    against an easy AI *)
let untimed_easy () = 
  let init_board = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 init_board;
  ANSITerminal.(print_string[green] "    |  0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
  print_endline "";
  print_string "    ";
  Static.print_line(40);
  print_endline "";
  Static.pp_arr 0 0 init_board;
  print_endline "It's player red's turn. \n";
  let start_time = Unix.time() in 
  print_endline "Type in a command to begin";
  print_string "> ";
  match read_line () with
  | command_str -> play_game_black_easy_ai init_board command_str start_time

(** [easy_ai] provides the interface for the game after a game against an easy AI
    has been selected by the user *)
let rec easy_ai () = 
  ANSITerminal.(print_string [cyan]
                  "\nWelcome to the easy game against our AI.\n");
  print_endline "";
  ANSITerminal.(print_string [yellow]
                  "\n\nChoose timed or untimed");
  print_endline "";
  ANSITerminal.(print_string [yellow]
                  "Type timed for a timed game and untimed for an untimed game");
  print_endline "";
  print_string ">";
  let opt = Stdlib.read_line () in
  match opt with 
  | "timed" -> timed_easy ()
  | "untimed" -> untimed_easy () 
  | _ -> print_endline "Incorrect input. Try again!"; easy_ai()  




(** Timed - Easy AI *)
(** [play_timed_game_black_hard_ai board str start_time legnth] provides the interface for a timed game 
    against a hard AI for when it is player black's turn *)
let rec play_timed_game_black_hard_ai board str start_time length = 
  let res = State.execute_command_time board str "black" start_time length in
  match res with
  | State x ->
    (  ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
       print_endline "";
       print_string "    ";
       Static.print_line(40);
       print_endline "";
       Static.pp_arr 0 0 x;
       Stdlib.print_string "\n";
       Stdlib.print_string "It is player white's turn\n";
       if ((State.pos_moves_white x 7 7 []) = []) then (Stdlib.print_string "\n";
                                                        Stdlib.print_string "No more valid moves for white player. Red has won!\n"; Stdlib.exit 0) else
         play_timed_game_white_hard_ai x start_time length)
  | Quit_Game -> ANSITerminal.(print_string[cyan] "Thank you for playing with us!");print_endline "";Stdlib.exit 0
  | Hint_Game -> (
      let lst = State.pos_moves_black board 7 7 [] in
      ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
      print_endline "";
      print_string "    ";
      Static.print_line(40);
      print_endline "";
      let t_brd = State.make_copy board in
      let n_brd = color_brd (t_brd) lst in
      Static.pp_arr 0 0 n_brd;
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_timed_game_black_hard_ai board com_str start_time length
    )
  | Help_Game -> (
      print_endline "";
      ANSITerminal.(print_string [white]
                      "Let me give you a basic rundown of our game.
                  We have two game options: to play agaisnt another person or to play
                  against our computer. We also have the option of timed and untimed checkers.
                  Timed checkers will run the game for the specified amount of time and will end
                  as soon as the time is up. On the other hand, untimed will run till someone 
                  wins or typed quit.
                  - quit : typed when you want to exit the game
                  - hint : typed when you want to see all the possible moves you can make
                  - help : to show all this information all over again.
                  Have a fun game!");
      print_endline "";
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_timed_game_black_hard_ai board com_str start_time length

    )
  | Illegal -> (
      Stdlib.print_string "\n";
      Stdlib.print_string "Illegal move, try again & type in a new command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_timed_game_black_hard_ai board com_str start_time length)

(** [play_timed_game_white_hard_ai board str start_time legnth] provides the interface for a timed game 
    against a hard AI for when it is player white's turn *)
and play_timed_game_white_hard_ai board (start_time:float) length = 
  let t_board = State.make_copy board in
  let str1 = State.get_best_move t_board in
  let res = State.execute_command_time board str1 "white" start_time length in
  match res with
  | State x ->
    (  ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
       print_endline "";
       print_string "    ";
       Static.print_line(40);
       print_endline "";
       Static.pp_arr 0 0 x;
       Stdlib.print_string "\n";
       Stdlib.print_string "It is player red's turn\n";
       if ((State.pos_moves_black x 7 7 []) = []) then (Stdlib.print_string "\n";
                                                        Stdlib.print_string "No more valid moves for red player. White has won!\n"; Stdlib.exit 0) else
         Stdlib.print_string "Type in another command.\n";
       print_string "> ";
       let com_str = Stdlib.read_line(); in
       play_timed_game_black_hard_ai x com_str start_time length)
  | Quit_Game -> ANSITerminal.(print_string[cyan] "Thank you for playing with us!");print_endline "";Stdlib.exit 0
  | Hint_Game -> (
      let lst = State.pos_moves_black board 7 7 [] in
      ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
      print_endline "";
      print_string "    ";
      Static.print_line(40);
      print_endline "";
      let t_brd = State.make_copy board in
      let n_brd = color_brd (t_brd) lst in
      Static.pp_arr 0 0 n_brd;
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_timed_game_white_hard_ai board start_time length
    )
  | Help_Game -> (
      print_endline "";
      ANSITerminal.(print_string [white]
                      "Let me give you a basic rundown of our game.
                  We have two game options: to play agaisnt another person or to play
                  against our computer. We also have the option of timed and untimed checkers.
                  Timed checkers will run the game for the specified amount of time and will end
                  as soon as the time is up. On the other hand, untimed will run till someone 
                  wins or typed quit.
                  - quit : typed when you want to exit the game
                  - hint : typed when you want to see all the possible moves you can make
                  - help : to show all this information all over again.
                  Have a fun game!");
      print_endline "";
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_timed_game_white_hard_ai board start_time length

    )
  | Illegal -> (
      Stdlib.print_string "\n";
      Stdlib.print_string "Illegal move, try again & type in a new command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_timed_game_white_hard_ai board start_time length)
(** End of timed-medium-Ai *)

(** Untimed - Medium AI *)
(** [play_game_black_hard_ai board str start_time] provides the interface for an utimed game 
    against a hard AI for when it is player black's turn *)
let rec play_game_black_hard_ai board str start_time = 
  let res = State.execute_command board str "black" start_time in
  match res with
  | State x ->
    (  ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
       print_endline "";
       print_string "    ";
       Static.print_line(40);
       print_endline "";
       Static.pp_arr 0 0 x;
       Stdlib.print_string "\n";
       Stdlib.print_string "It is player white's turn\n";
       (* Stdlib.print_string "Type in another command.\n";
          print_string "> ";
          let com_str = Stdlib.read_line(); in *)
       if ((State.pos_moves_white x 7 7 []) = []) then (Stdlib.print_string "\n";
                                                        Stdlib.print_string "No more valid moves for white player. Red has won!\n"; Stdlib.exit 0) else
         (* Stdlib.print_string "Type in another command.\n";
            print_string "> ";
            let com_str = Stdlib.read_line(); in *)
         play_game_white_hard_ai x start_time) 
  | Quit_Game -> ANSITerminal.(print_string[cyan] "Thank you for playing with us!");print_endline "";Stdlib.exit 0
  | Hint_Game -> (
      let lst = State.pos_moves_black board 7 7 [] in
      ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
      print_endline "";
      print_string "    ";
      Static.print_line(40);
      print_endline "";
      let t_brd = State.make_copy board in
      let n_brd = color_brd (t_brd) lst in
      Static.pp_arr 0 0 n_brd;
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_game_black_hard_ai board com_str start_time 
    )
  | Help_Game -> (
      print_endline "";
      ANSITerminal.(print_string [white]
                      "Let me give you a basic rundown of our game.
                  We have two game options: to play agaisnt another person or to play
                  against our computer. We also have the option of timed and untimed checkers.
                  Timed checkers will run the game for the specified amount of time and will end
                  as soon as the time is up. On the other hand, untimed will run till someone 
                  wins or typed quit.
                  - quit : typed when you want to exit the game
                  - hint : typed when you want to see all the possible moves you can make
                  - help : to show all this information all over again.
                  Have a fun game!");
      print_endline "";
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_game_black_hard_ai board com_str start_time 

    )
  | Illegal -> (
      Stdlib.print_string "\n";
      Stdlib.print_string "Illegal move, try again & type in a new command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_game_black_hard_ai board com_str start_time)

(** [play_game_white_hard_ai board str start_time] provides the interface for an utimed game 
    against a hard AI for when it is player white's turn *)
and play_game_white_hard_ai board start_time = 
  let t_board = State.make_copy board in
  let str1 = State.get_best_move t_board in
  let res = State.execute_command board str1 "white" start_time in
  match res with
  | State x ->
    (  ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
       print_endline "";
       print_string "    ";
       Static.print_line(40);
       print_endline "";
       Static.pp_arr 0 0 x;
       Stdlib.print_string "\n";
       Stdlib.print_string "It is player red's turn\n";
       if ((State.pos_moves_black x 7 7 []) = []) then (Stdlib.print_string "\n";
                                                        Stdlib.print_string "No more valid moves for red player. White has won!\n"; Stdlib.exit 0) else
         Stdlib.print_string "Type in another command.\n";
       print_string "> ";
       let com_str = Stdlib.read_line(); in
       play_game_black_hard_ai x com_str start_time)
  | Quit_Game -> ANSITerminal.(print_string[cyan] "Thank you for playing with us!");print_endline "";Stdlib.exit 0
  | Hint_Game -> (
      let lst = State.pos_moves_black board 7 7 [] in
      ANSITerminal.(print_string[green] "    |   0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
      print_endline "";
      print_string "    ";
      Static.print_line(40);
      print_endline "";
      let t_brd = State.make_copy board in
      let n_brd = color_brd (t_brd) lst in
      Static.pp_arr 0 0 n_brd;
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_game_white_hard_ai board start_time 
    )
  | Help_Game -> (
      print_endline "";
      ANSITerminal.(print_string [white]
                      "Let me give you a basic rundown of our game.
                  We have two game options: to play agaisnt another person or to play
                  against our computer. We also have the option of timed and untimed checkers.
                  Timed checkers will run the game for the specified amount of time and will end
                  as soon as the time is up. On the other hand, untimed will run till someone 
                  wins or typed quit.
                  - quit : typed when you want to exit the game
                  - hint : typed when you want to see all the possible moves you can make
                  - help : to show all this information all over again.
                  Have a fun game!");
      print_endline "";
      Stdlib.print_string "Type in a command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_game_white_hard_ai board start_time 

    )
  | Illegal -> (
      Stdlib.print_string "\n";
      Stdlib.print_string "Illegal move, try again & type in a new command.\n";
      print_string "> ";
      let com_str = Stdlib.read_line(); in
      play_game_white_hard_ai board start_time)

(** End of Untimed med- AI *)
(** [how_long_hard length] provides the interface after a game against a hard AI has been
    selected and the uswer has entered how long they want to play the game for*)
let how_long_hard length = 
  let init_board = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 init_board;
  ANSITerminal.(print_string[green] "    |  0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
  print_endline "";
  print_string "    ";
  Static.print_line(40);
  print_endline "";
  Static.pp_arr 0 0 init_board;
  print_endline "It's player red's turn. \n";
  let start_time = Unix.time() in 
  print_endline "Type in a command to begin";
  print_string "> ";
  match read_line () with
  | command_str -> play_timed_game_black_hard_ai init_board command_str start_time length

(** [timed_game] provides the interface prompting the user how long they want to
    play the game for after they have selected a game against a hard AI *)
let timed_hard () = 
  ANSITerminal.(print_string [yellow]
                  "Type the length of the game in seconds.");
  print_endline "";
  print_string ">";
  let opt = Stdlib.read_line () in
  how_long_hard (float_of_string opt)

(** [untimed_game] provides the interface for the user after they have selected an untimed 
    game against a hard AI *)
let untimed_hard () = 
  let init_board = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 init_board;
  ANSITerminal.(print_string[green] "    |  0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
  print_endline "";
  print_string "    ";
  Static.print_line(40);
  print_endline "";
  Static.pp_arr 0 0 init_board;
  print_endline "It's player red's turn. \n";
  let start_time = Unix.time() in 
  print_endline "Type in a command to begin";
  print_string "> ";
  match read_line () with
  | command_str -> play_game_black_hard_ai init_board command_str start_time

(** [hard_ai] provides the interface for the user after they have selected a game against a hard AI *)
let rec hard_ai () = 
  ANSITerminal.(print_string [cyan]
                  "\nWelcome to the hard game against our AI.\n");
  print_endline "";
  ANSITerminal.(print_string [yellow]
                  "\n\nChoose timed or untimed");
  print_endline "";
  ANSITerminal.(print_string [yellow]
                  "Type timed for a timed game and untimed for an untimed game");
  print_endline "";
  print_string ">";
  let opt = Stdlib.read_line () in
  match opt with 
  | "timed" -> timed_hard ()
  | "untimed" -> untimed_hard () 
  | _ -> print_endline "Incorrect input. Try again!"; hard_ai()





(** [single_player] provides the interface for a single player game *)
let rec single_player() = 
  ANSITerminal.(print_string [yellow]
                  "\n\nChoose your level: Easy, medium or hard?");
  print_endline "";
  ANSITerminal.(print_string [yellow]
                  "Type eai for easy, mai for medium and hai for hard");
  print_endline "";
  print_string ">";
  let opt = Stdlib.read_line () in
  match opt with 
  | "eai" -> easy_ai ()
  | "mai" -> medium_ai () 
  | "hai" -> hard_ai ()
  | _ -> print_endline "Incorrect input. Try again!"; single_player()  

(** [multiplayer] provides the interface for a multi player game *)
let multiplayer () = 
  ANSITerminal.(print_string [cyan]
                  "\nGame begins.\n");
  print_endline "";
  let init_board = Static.create_board 8 8 in
  Static.create_board_init_h 8 8 init_board;
  ANSITerminal.(print_string[green] "    |  0 | 1  | 2  | 3  | 4  | 5  | 6  | 7  |");
  print_endline "";
  print_string "    ";
  Static.print_line(40);
  print_endline "";
  Static.pp_arr 0 0 init_board;
  print_endline "It's player red's turn. \n";
  let start_time = Unix.time() in 
  print_endline "Type in a command to begin";
  print_string "> ";
  match read_line () with
  | command_str -> play_game_black init_board command_str start_time

(** [main] provides the beginning interface for when the checkers game if first started *)
let rec main () = 
  (* let temp_st_lst = [cyan;on_blue] in *)
  ANSITerminal.(print_string [cyan]
                  "\n\n Welcome to Checkers.\n");
  print_endline "";
  ANSITerminal.(print_string [white]
                  "Let me give you a basic rundown of our game.
                  We have two game options: to play agaisnt another person or to play
                  against our computer. We also have the option of timed and untimed checkers.
                  Timed checkers will run the game for the specified amount of time and will end
                  as soon as the time is up. On the other hand, untimed will run till someone 
                  wins or types quit.
                  - quit : type when you want to exit the game
                  - hint : type when you want to see all the possible moves you can make
                  - help : to show all this information all over again.
                  Have a fun game!\n");
  print_endline "";
  ANSITerminal.(print_string [yellow]
                  "Do you want to play single player or multiplyer?");
  print_endline "";
  ANSITerminal.(print_string [yellow]
                  "Type 1 for single player or 2 for multiplayer.");
  print_endline "";
  print_string ">";
  let opt = Stdlib.read_line() in
  match int_of_string opt with 
  | 1 -> single_player ()
  | 2 -> multiplayer ()
  | _ -> print_endline "Incorrect input. Try again!"; main()


let () = main ()