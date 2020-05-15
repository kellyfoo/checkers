open Array
open Static
open Command
open Unix

(** [scoreBlack] is the reference created to check the number of pieces Black piece has captured 
    and is initialised to 0*)
let scoreBlack = ref 0

(** [scoreWhite] is the reference created to check the number of pieces White piece has captured 
    and is initialised to 0*)
let scoreWhite = ref 0

(** result is the return type of function execute_command *)
type result =
  | State of (Static.piece array array)
  | Timed of (Static.piece array array)
  | Hint_Game
  | Help_Game
  | Quit_Game
  | Illegal

(** Copies the piece present in location (row,col) in old board to the new board at the same
    location.*)
let update_n_matrix_piece old_brd new_brd row col = 
  new_brd.(row).(col) <- old_brd.(row).(col);
  new_brd


(** Updates a row in the matrix using the update_n_matrix_piece *)
let rec update_n_matrix_row old_brd new_brd row cols = 
  match cols with 
  | 0 -> update_n_matrix_piece old_brd new_brd row 0 
  | c -> update_n_matrix_row old_brd (update_n_matrix_piece old_brd new_brd row c) row (cols-1)

(** [update_n_matrix old_brd new_brd row cols] updates the [new_brd] using the update_n_matrix_row function*)
let rec update_n_matrix old_brd new_brd rows cols =
  match rows with 
  | 0 -> update_n_matrix_row old_brd new_brd 0 cols 
  | r -> update_n_matrix old_brd (update_n_matrix_row old_brd new_brd r cols) (r-1) cols 

(** [make_copy brd] Creates a copy of the [brd] which has a new memory location.
    Requires: A valid Static.piece array array  *)
let make_copy brd = 
  let t_matrix = Static.create_board 8 8 in
  let fin = update_n_matrix brd t_matrix 7 7 in 
  fin

(** [create_new_piece old_piece new_x new_y] returns a piece with the same
    color as the old piece with updated coordinate values (new_x, new_y)
    Requires: [old_piece] is of type piece and [new_x] and [new_y] are of 
    type int*)
let create_new_piece old_piece new_x new_y = 
  match old_piece with
  | Normal (Black, _, i1) ->
    (if new_x = 7 
     then (King (Black, Some (new_x, new_y), 5))
     else Normal (Black, Some (new_x, new_y),3))
  | Normal (White, _, i2) -> 
    (if new_x = 0 then King (White, Some (new_x, new_y), 5)
     else Normal (White, Some (new_x, new_y),3))
  | King (c, _, i3) -> King (c, Some(new_x,new_y),5)
  | Nil -> Nil

(**[remove_piece board old_x old_y] removes the piece with those specific coordinates
   from the board and replaces it with a Nil type. *)
let remove_piece board old_x old_y = 
  board.(old_x).(old_y) <- Nil

(**[check_piece board x y] checks if the piece at position (x,y) is of a type and returns
   a string value assosciated with the type. *)
let check_piece board x y = 
  match board.(x).(y) with
  | Nil -> "Nil"
  | Normal(Black,_,_) -> "black-normal"
  | Normal (White,_,_) -> "white-normal"
  | King (Black,_,_) -> "black-king"
  | King (White,_,_) -> "white-king"

(**[check_piece_piece piece] checks if piece is of a type and returns a string value
   associated with the type. *)
let check_piece_piece piece = 
  match piece with
  | Nil -> "Nil"
  | Normal(Black,_,_) -> "black-normal"
  | Normal (White,_,_) -> "white-normal"
  | King (Black,_,_) -> "black-king"
  | King (White,_,_) -> "white-king"

(** [can_jump_to board x2 y2] checks to see that there is no piece at (x2,y2)
    of the board so that another piece can move to it
    Requires: [board] is of type 2-d piece array, [x2] and [y2] are of type int *)
let can_jump_to board x2 y2 = 
  match board.(x2).(y2) with 
  | Nil -> true 
  | _ -> false

(** [valid_jump board x1 y1 x2 y2] returns if jumping from x1 y1 to x2 y2 on the board
    is valid - also returns and quits the game if one player has reached 12 points *)
let valid_jump board x1 y1 x2 y2 =
  let piece_betw = board.((x1+x2)/2).((y1+y2)/2) in
  match piece_betw,board.(x1).(y1) with
  | Nil,_ -> Illegal
  | Normal(Black,_,_),Normal(White,_,_) -> (
      scoreWhite := !scoreWhite + 1;
      let white_string = string_of_int (!scoreWhite) in
      let black_string = string_of_int (!scoreBlack) in
      print_endline("\nWhite Score: " ^ white_string);
      print_endline ("Red Score: " ^ black_string^ "\n");
      if (scoreWhite = ref 12) then (Stdlib.print_string "\nPlayer white has won!\n"; Stdlib.exit 0)
      else State board
    )
  | Normal(White,_,_),Normal(Black,_,_) -> (
      scoreBlack := !scoreBlack + 1;
      let white_string = string_of_int (!scoreWhite) in
      let black_string = string_of_int (!scoreBlack) in
      print_endline("\nWhite Score: " ^ white_string);
      print_endline ("Red Score: " ^ black_string^ "\n");
      if (scoreBlack = ref 12) then (Stdlib.print_string "\nPlayer red has won!\n"; Stdlib.exit 0)
      else State board
    )
  | King (Black,_,_),Normal(White,_,_) -> (
      scoreWhite := !scoreWhite + 1;
      let white_string = string_of_int (!scoreWhite) in
      let black_string = string_of_int (!scoreBlack) in
      print_endline("\nWhite Score: " ^ white_string);
      print_endline ("Black Score: " ^ black_string^ "\n");
      if (scoreWhite = ref 12) then (Stdlib.print_string "\nPlayer white has won!\n"; Stdlib.exit 0)
      else State board
    ) 
  | King(White,_,_),Normal(Black,_,_) -> (
      scoreBlack := !scoreBlack + 1;
      let white_string = string_of_int (!scoreWhite) in
      let black_string = string_of_int (!scoreBlack) in
      print_endline("\nWhite Score: " ^ white_string);
      print_endline ("Red Score: " ^ black_string^ "\n");
      if (scoreBlack = ref 12) then (Stdlib.print_string "\nPlayer red has won!\n"; Stdlib.exit 0)
      else State board
    )
  | Normal(White,_,_),King(Black,_,_) ->(
      scoreBlack := !scoreBlack + 1;
      let white_string = string_of_int (!scoreWhite) in
      let black_string = string_of_int (!scoreBlack) in
      print_endline("\nWhite Score: " ^ white_string);
      print_endline ("Red Score: " ^ black_string^ "\n");
      if (scoreBlack = ref 12) then (Stdlib.print_string "\nPlayer red has won!\n"; Stdlib.exit 0)
      else State board
    )
  | Normal(Black,_,_),King(White,_,_) ->(
      scoreWhite := !scoreWhite + 1;
      let white_string = string_of_int (!scoreWhite) in
      let black_string = string_of_int (!scoreBlack) in
      print_endline("\nWhite Score: " ^ white_string);
      print_endline ("Red Score: " ^ black_string^ "\n");
      if (scoreWhite = ref 12) then (Stdlib.print_string "\nPlayer white has won!\n"; Stdlib.exit 0)
      else State board
    ) 
  | _, _ -> Illegal

(** [is_jump x1 y1 x2 y2] return if the move is a jump
    Requires: [x1], [x2], [y1], [y2] are of type int*)
let is_jump x1 y1 x2 y2 = (abs (x1-x2) = 2) && (abs (y1-y2) = 2)

(**[valid_move board x1 y1 x2 y2] checks if the normal piece move up and down properly
   depending on the color. Black moves up the array i.e the row number increases and 
   white moves down the array i.e the row number decreases.
   It also checks if the new coordinate is in bounds or not and if the king pieces are movinng
   properly along all its diagonals.*)
let valid_move board x1 y1 x2 y2 = 
  let is_x_range = x2>=0 && x2<=7 && x1>=0 && x2<=7 in
  let is_y_range = y2>=0 && y2<=7 && x1>=0 && x2<=7 in
  let ev_range = is_x_range && is_y_range in
  if ev_range == false then Illegal
  else
    let empty_dest = can_jump_to board x2 y2 in
    let piece = board.(x1).(y1) in
    match piece,ev_range,empty_dest with
    | Normal (Black, Some (x,y),_),true,true -> 
      (if x2 - x1 = 1 && abs (y1 - y2) = 1 && can_jump_to board x2 y2 then State board
       else if x2 - x1 = 2 && abs (y1 - y2) = 2 then valid_jump board x1 y1 x2 y2
       else (Illegal ))
    | Normal (White, Some (x,y),_),true,true ->
      (if x1 - x2 = 1 && abs (y1 - y2) = 1 && can_jump_to board x2 y2 then State board
       else if x1 - x2 = 2 && abs (y1 - y2) = 2 then valid_jump board x1 y1 x2 y2
       else (Illegal ))
    | King (_,_,_),true,true -> 
      (if abs (x1-x2) = 1 && abs (y1-y2) = 1 && can_jump_to board x2 y2 then 
         (State board) 
       else if abs (x1-x2) = 2 && abs (y1-y2) = 2 then
         (valid_jump board x1 y1 x2 y2)
       else (Illegal ))
    | Nil,_,_ -> (Illegal)
    | _,_,_ -> ( Illegal)

(** [move_standard board x1 y1 x2 y2] moves the piece at (x1, y1) of board to 
    (x2, y2) of board
    Requires: [board] is of type 2-d piece array, [x1], [x2], [y1], [y2] are of
              type int and the difference between the two x and y coordinates are 2*)
let move_standard board x1 y1 x2 y2 = 
  let old_piece = board.(x1).(y1) in
  board.(x2).(y2) <- create_new_piece old_piece x2 y2;
  remove_piece board x1 y1;
  State board

(** [move_jump board x1 y1 x2 y2] *)
let move_jump board x1 y1 x2 y2 = 
  let new_piece = create_new_piece board.(x1).(y1) x2 y2 in
  (* print_endline (check_piece_piece new_piece ); *)
  board.(x2).(y2) <- new_piece;
  remove_piece board ((x1+x2)/2) ((y1+y2)/2);
  remove_piece board x1 y1;
  State board

(** [get_color piece] returns the color of the piece
    Requires: [piece] is of type piece *)
let get_color piece =
  match piece with 
  | Normal (Black, _, _) -> "black"
  | King (Black, _, _) -> "black"
  | Normal (White, _, _) -> "white"
  | King (White, _, _) -> "white"
  | _ -> "wrong"

(** [move_board board x1 y1 x2 y2 color] moves the piece at position (x1, y1) of
    board to position (x2, y2) if it is a legal move
    Requires: [board] is of type 2-d piece array, [x1], [y1], [x2], and [y2] are
              of type int *)
let move_board board x1 y1 x2 y2 color = 
  let piece = board.(x1).(y1) in
  if get_color piece <> color then Illegal else 
    let valid = valid_move board x1 y1 x2 y2 in
    let jump = is_jump x1 y1 x2 y2 in
    match valid, jump with
    | State _, false -> move_standard board x1 y1 x2 y2
    | State _, true -> move_jump board x1 y1 x2 y2
    | _ -> Illegal

(** [valid_jump_bool_black board x1 y1 x2 y2] checks if for the black piece at ([x1],[y1]),
    if it can jump to the position ([x2],[y2]).
    Requires:- A valid Static.piece array array an 4 valid integers
    Returns:- A boolean value*)
let valid_jump_bool_black board x1 y1 x2 y2 =
  let piece_betw = board.((x1+x2)/2).((y1+y2)/2) in
  match piece_betw,board.(x1).(y1) with
  | Nil,_ -> false
  | Normal(White,_,_),Normal(Black,_,_) -> true
  | King(White,_,_),Normal(Black,_,_) -> true
  | Normal(White,_,_),King(Black,_,_) ->true
  | King(White,_,_), King(Black,_,_) -> true
  | _, _ -> false


(** [valid_jump_bool_black board x1 y1 x2 y2] checks if for the white piece at ([x1],[y1]),
    if it can jump to the position ([x2],[y2]).
    Requires:- A valid Static.piece array array an 4 valid integers
    Returns:- A boolean value*)
let valid_jump_bool_white board x1 y1 x2 y2 =
  let piece_betw = board.((x1+x2)/2).((y1+y2)/2) in
  match piece_betw,board.(x1).(y1) with
  | Nil,_ -> false
  | Normal(Black,_,_),Normal(White,_,_) -> true
  (* | Normal(White,_,_),Normal(Black,_,_) -> true *)
  | King (Black,_,_),Normal(White,_,_) -> true
  (* | King(White,_,_),Normal(Black,_,_) -> true
     | Normal(White,_,_),King(Black,_,_) ->true *)
  | Normal(Black,_,_),King(White,_,_) ->true
  | King (Black,_,_), King(White,_,_) -> true
  | _, _ -> false

(** [valid_jump_bool_black board x1 y1 x2 y2] checks if for the black piece at ([x1],[y1]),
    if it can move to the position ([x2],[y2]).
    Requires:- A valid Static.piece array array an 4 valid integers
    Returns:- A boolean value*)
let valid_move_bool_black board x1 y1 x2 y2 = 
  let is_x_range = x2>=0 && x2<=7 in
  let is_y_range = y2>=0 && y2<=7 in
  let ev_range = is_x_range && is_y_range in
  let empty_dest = if ev_range = false then false else can_jump_to board x2 y2 in
  let piece = board.(x1).(y1) in
  match piece,ev_range,empty_dest with
  | _,false,false -> false
  | Normal (Black, Some (x,y),_),true,true -> (
      (* (  print_endline "it is a normal black piece"; *)
      if x2 - x1 = 1 && abs (y1 - y2) = 1 && can_jump_to board x2 y2 then true
      else if x2 - x1 = 2 && abs (y1 - y2) = 2 then valid_jump_bool_black board x1 y1 x2 y2
      else false)
  | King (Black,_,_),true,true -> 
    (
      (* print_endline "it is a king black piece"; *)
      if abs (x1-x2) = 1 && abs (y1-y2) = 1 && can_jump_to board x2 y2 then 
        true
      else if abs (x1-x2) = 2 && abs (y1-y2) = 2 then
        (
          valid_jump_bool_black board x1 y1 x2 y2)
      else false)
  | Nil,_,_ -> false
  | _,_,_ -> false



(** [valid_jump_bool_black board x1 y1 x2 y2] checks if for the white piece at ([x1],[y1]),
    if it can move to the position ([x2],[y2]).
    Requires:- A valid Static.piece array array an 4 valid integers
    Returns:- A boolean value*)
let valid_move_bool_white board x1 y1 x2 y2 = 
  let is_x_range = x2>=0 && x2<=7 in
  (* print_endline ("is it in range of rows " ^ string_of_bool is_x_range); *)
  let is_y_range = y2>=0 && y2<=7 in
  (* print_endline ("is it in range of columns " ^ string_of_bool is_y_range); *)
  let ev_range = is_x_range && is_y_range in
  let empty_dest = if ev_range = false then false else can_jump_to board x2 y2 in
  (* print_endline ("can it jump to new coords " ^ string_of_bool empty_dest); *)
  let piece = board.(x1).(y1) in
  match piece,ev_range,empty_dest with
  | _,false,false -> false
  | Normal (White, Some (x,y),_),true,true ->
    ( 
      if x1 - x2 = 1 && abs (y1 - y2) = 1 && can_jump_to board x2 y2 then true
      else if x1 - x2 = 2 && abs (y1 - y2) = 2 then valid_jump_bool_white board x1 y1 x2 y2
      else false)
  | King (White,_,_),true,true -> 
    (
      if abs (x1-x2) = 1 && abs (y1-y2) = 1 && can_jump_to board x2 y2 then 
        true
      else if abs (x1-x2) = 2 && abs (y1-y2) = 2 then
        (
          valid_jump_bool_white board x1 y1 x2 y2)
      else false)
  | Nil,_,_ -> false
  | _,_,_ -> false


(**[pos_moves_piece brd row coll acc color] returns the [acc] with new tuples of beginning
   and end coordinates prepended to it.
   Requires:- A valid matrix, 2 valid integers to specify the size, a valid 'a list and a valid string
   Returns:- A valid list*)
let rec pos_moves_piece brd row col acc color = 
  match color with 
  | "white" -> (
      (* print_endline "it is a white piece"; *)
      let acc1 = if valid_move_bool_white brd row col (row-1) (col-1) then ((row,col),(row-1,col-1))::acc else acc in
      let acc2 = if valid_move_bool_white brd row col (row-1) (col+1) then ((row,col),(row-1,col+1))::acc1 else acc1 in
      let acc3 = if valid_move_bool_white brd row col (row+1) (col-1) then ((row,col),(row+1,col-1))::acc2 else acc2 in
      let acc4 = if valid_move_bool_white brd row col (row+1) (col+1) then ((row,col),(row+1,col+1))::acc3 else acc3 in
      let acc5 = if valid_move_bool_white brd row col (row+2)(col+2) then ((row,col),(row+2,col+2))::acc4 else acc4 in
      let acc6 = if valid_move_bool_white brd row col (row+2)(col-2) then ((row,col),(row+2,col-2))::acc5 else acc5 in
      let acc7 = if valid_move_bool_white brd row col (row-2)(col+2) then ((row,col),(row-2,col+2))::acc6 else acc6 in
      let acc8 = if valid_move_bool_white brd row col (row-2)(col-2) then ((row,col),(row-2,col-2))::acc7 else acc7 in
      acc8
    ) 
  | _ -> acc

(** [pos_moves_row brd row cols acc color] parses the [row] of the given [brd] and returns 
    an [acc] (a list of possible moves) for all the pieces present in the given [row]

    Requires:- A valid matrix, 2 integers to signify the size of the matrix, an 'a list and a string
    Returns:- an 'a list*)
let rec pos_moves_row brd row cols acc color = 
  match cols with 
  | 0 -> ( (**print_endline "we hit the last column";**)
      pos_moves_piece brd row 0 acc color)
  | c -> (**print_endline ("we are in " ^ string_of_int c ^ " column");**)
    pos_moves_row brd (row) (c-1) (pos_moves_piece brd row c acc color) color


(** [pos_moves_white brd row cols acc] pareses through all the [rows] of the given [brd] and 
    returns an [acc](a list of possible moves) for all the white pieces on the board.

    Requires:- A valid matrix, 2 integers to signify the size of the matrix and an 'a list.
    Returns :- An 'a list *)
let rec pos_moves_white brd rows cols acc = 
  (* print_endline "we are in pos_move_white function"; *)
  match rows with 
  | 0 -> (
      (**print_endline "we hit the last row";**)
      pos_moves_row brd 0 cols acc "white")
  | r -> ( (**print_endline ("we are in " ^ string_of_int r ^ " row";)**)
      pos_moves_white brd (r-1) cols (pos_moves_row brd r cols acc "white"))


(**[pos_moves_piece_black brd row coll acc ] returns the [acc] with new tuples of beginning
   and end coordinates prepended to it.
   Requires:- A valid matrix, 2 valid integers to specify the size and a valid 'a list 
   Returns:- A valid list*)
let pos_moves_piece_black board row col acc = 
  let acc1 = if valid_move_bool_black board row col (row+1)(col+1) = true then ((row,col),(row+1,col+1))::acc else acc in
  let acc2 = if valid_move_bool_black board row col (row+1)(col-1) = true then ((row,col),(row+1,col-1))::acc1 else acc1 in
  let acc3 = if valid_move_bool_black board row col (row-1)(col+1) = true then ((row,col),(row-1,col+1))::acc2 else acc2 in
  let acc4 = if valid_move_bool_black board row col (row-1)(col-1) = true then ((row,col),(row-1,col-1))::acc3 else acc3 in
  let acc5 = if valid_move_bool_black board row col (row+2)(col+2) = true then ((row,col),(row+2,col+2))::acc4 else acc4 in
  let acc6 = if valid_move_bool_black board row col (row+2)(col-2) = true then ((row,col),(row+2,col-2))::acc5 else acc5 in
  let acc7 = if valid_move_bool_black board row col (row-2)(col+2) = true then ((row,col),(row-2,col+2))::acc6 else acc6 in
  let acc8 = if valid_move_bool_black board row col (row-2)(col-2) = true then ((row,col),(row-2,col-2))::acc7 else acc7 in
  acc8

(** [pos_moves_row_black brd row cols acc ] parses the [row] of the given [brd] and returns 
    an [acc] (a list of possible moves) for all the pieces present in the given [row]

    Requires:- A valid matrix, 2 integers to signify the size of the matrix and an 'a list 
    Returns:- an 'a list*)
let rec pos_moves_row_black brd row cols acc = 
  match cols with 
  | 0 -> pos_moves_piece_black brd row 0 acc
  | c -> pos_moves_row_black brd row (c-1) (pos_moves_piece_black brd row c acc)


(** [pos_moves_black brd row cols acc] pareses through all the [rows] of the given [brd] and 
    returns an [acc](a list of possible moves) for all the white pieces on the board.

    Requires:- A valid matrix, 2 integers to signify the size of the matrix and an 'a list.
    Returns :- An 'a list *)
let rec pos_moves_black brd rows cols acc = 
  match rows with 
  | 0 -> pos_moves_row_black brd 0 cols acc 
  | r -> pos_moves_black brd (r-1) cols (pos_moves_row_black brd r cols acc)

(** [med_move_white board] returns a random move from the list of all possible moves
    that could be made by a white piece in the current [board] configuration.

    Requires:- A valid 2d matrix
    Returns:- A tuple of coordinates *)
let med_move_white board = 
  let white_moves = pos_moves_white board 7 7 [] in 
  let len = List.length white_moves in 
  let move = List.nth white_moves (Random.int len) in
  move

(** [med_move_black board] returns a random move from the list of all possible moves
    that could be made by a black piece in the current [board] configuration.

    Requires:- A valid 2d matrix
    Returns:- A tuple of coordinates *)
let med_move_black board = 
  let black_moves = pos_moves_black board 7 7 [] in 
  let len = List.length black_moves in 
  let move = List.nth black_moves (Random.int len) in
  move

(**[move_med board color] returns the medium AI move from the current [board] configuration
   for the given [color].
   Requires:- A valid 2d matrix and a valid string
   Returns:- A tuple of coordinates *)
let move_med board color = 
  match color with 
  | "white" -> med_move_white board 
  | "black" -> med_move_black board
  | _ -> med_move_white board 


(** [eval_piece_white board row col] evaluates the value of a white piece 
    depending upon its type and the [row] it is present in.
    Requires:- A valid 2d matrix and 2 valid integers to represent the particular position
    Returns:- an integer*)
let eval_piece_white board row col = 
  match board.(row).(col) with 
  | Normal(White,_,_) -> 5 + row
  | King (White,_,_) -> 5 + (7-row) + 2
  | _ -> 0

(** [eval_piece_black board row col] evaluates the value of a black piece 
    depending upon its type and the [row] it is present in.
    Requires:- A valid 2d matrix and 2 valid integers to represent the particular position
    Returns:- an integer*)
let eval_piece_black board row col = 
  match board.(row).(col) with 
  | Normal(Black,_,_) -> 5 + row
  | King (Black,_,_) -> 5 + (row-0) + 2
  | _ -> 0

(** [eval_row_white board row cols acc] evaluates the total score of [row] from the perspective
    of the white player playing.
    Requires:- A valid 2d matrix, 2 integers to represent the row and the total number of columns
    and a valid integer which stores the value of an entire row.
    Returns:- A valid integer *)
let rec eval_row_white board row cols acc= 
  match cols with 
  | 0 -> acc + (eval_piece_white board row 0)
  | x -> eval_row_white board row (cols-1) (acc + (eval_piece_white board row x))

(** [eval_row_black board row cols acc] evaluates the total score of [row] from the perspective
    of the black player playing.
    Requires:- A valid 2d matrix, 2 integers to represent the row and the total number of columns
    and a valid integer which stores the value of an entire row.
    Returns:- A valid integer *)
let rec eval_row_black board row cols acc= 
  match cols with 
  | 0 -> acc + (eval_piece_black board row 0)
  | x -> eval_row_black board row (cols-1) (acc + (eval_piece_black board row x))


(**[eval_board_white b rows cols total] returns the total value of the board when playing
   from the perspective of the white piece.
   Requires:- a valid matrix, 2 integers to represent the dimensions of the matrix and an integer
   accumulator.
   Returns:- an integer*)
let rec eval_board_white b rows cols total= 
  match rows with 
  | 0 -> eval_row_white b (0)(cols) (total)
  | x -> eval_board_white b (rows-1) (cols) (eval_row_white b x cols total)


(**[eval_board_black b rows cols total] returns the total value of the board when playing
   from the perspective of the black piece.
   Requires:- a valid matrix, 2 integers to represent the dimensions of the matrix and an integer
   accumulator.
   Returns:- an integer*)
let rec eval_board_black b rows cols total= 
  (* print_endline "we are in the eval_board_black funtion"; *)
  match rows with 
  | 0 -> eval_row_black b (0)(cols) (total)
  | x -> eval_board_black b (rows-1) (cols) (eval_row_black b x cols total)

(**[eval_board_fin b x1 x2 y1 y2 color] returns the total value of the board when a piece
   is moved from ([x1],[y1]) to ([x2],[y2]) and the evaluation is done from the perspective
   of [color] piece
   Requires:- a valid matrix, 4 integers to represent coordinates and a valid string
   accumulator.
   Returns:- an integer*)
let eval_board_fin b x1 x2 y1 y2 color = 
  let t_brd = make_copy b in
  move_board t_brd x1 x2 y1 y2;
  if color = "white" then eval_board_white t_brd 7 7 0 else eval_board_black t_brd 7 7 0

(**[create_pos_board b t1 t2 m1 m2 color] creates a copy of [b] and then moves a piece
   from ([t1],[t2]) to ([m1],[m2]).
   Requires:- A valid 2d matrix, 4 integers representing the coordinates and a valid string
   Returns:- A valid 2d matrix *)
let create_pos_board b t1 t2 m1 m2 color= 
  let t_brd = make_copy b in
  match color with 
  | "white" -> (if valid_move_bool_white t_brd t1 t2 m1 m2 then 
                  (
                    t_brd.(m1).(m2) <- t_brd.(t1).(t2);
                    t_brd.(t1).(t2) <- Nil;
                    t_brd
                  ) else t_brd)
  | "black" -> (if valid_move_bool_black t_brd t1 t2 m1 m2 then 
                  (
                    t_brd.(m1).(m2) <- t_brd.(t1).(t2);
                    t_brd.(t1).(t2) <- Nil;
                    t_brd
                  ) else t_brd)
  | _ -> t_brd



type tree = Root of int * tree list | Node of (int * (int *int) * (int*int)) * tree list

(** [create_nodes_white board lst acc] takes in the coords of possible moves of the white
    pieces from the [lst] and creates a type tree and returns [acc]
    Requires:- A valid 2d matrix, an 'a list and a tree list
    Returns:- A tree list*)
let rec create_nodes_white board lst (acc:tree list) = 
  match lst with 
  | [] -> acc
  | ((x1,y1),(x2,y2))::tl -> 
    ( let eval_brd = eval_board_white (create_pos_board board x1 y1 x2 y2 "white") 7 7 0 in
      let new_acc = Node((eval_brd,(x1,y1),(x2,y2)),[]) :: acc in
      create_nodes_white board tl new_acc)


(** [create_nodes_black board lst acc] takes in the coords of possible moves of the black
    pieces from the [lst] and creates a type tree and returns [acc]
    Requires:- A valid 2d matrix, an 'a list and a tree list
    Returns:- A tree list*)
let rec create_nodes_black board lst acc = 
  match lst with 
  | [] -> acc
  | ((x1,y1),(x2,y2))::tl -> 
    ( let eval_brd = eval_board_black (create_pos_board board x1 y1 x2 y2 "black") 7 7 0 in
      let new_acc = Node((eval_brd,(x1,y1),(x2,y2)),[]) :: acc in
      create_nodes_black board tl new_acc)

(** [create_t_brds board n color] creates temporary boards from [b] and [n] and evaluates it
    so that we can give a weight to each node of type tree.
    Requires:- A valid 2d matrix, a tuple of beginning and end coordinates and a valid string
    Returns:- an integer*)
let create_t_brds board n color = 
  match n with 
  |((x1,y1),(x2,y2)) -> eval_board_fin board x1 y1 x2 y2 color
  | _ -> -10000 

(** [make_tree] depth board acc makes a tree of the given [depth] using [board] as the root.
    Requires:- An int, a valid 2d matrix and a tree list
    Returns:- A tree list *)
let rec make_tree depth board acc :tree list = 
  match depth with 
  | 0 -> ( let t_lst1 = make_tree (depth+1) board [] in
           Root (eval_board_black board 7 7 0,t_lst1))::acc
  | 1 -> (
      let t_lst = pos_moves_white board 7 7 [] in
      let brd_lst = List.map (fun x -> create_t_brds board x "white") t_lst in
      let comb_lst = List.combine t_lst brd_lst in
      List.map (fun x -> create_nodes x 1 acc board) comb_lst)

  | 2 -> (
      let t_lst = pos_moves_black board 7 7 [] in
      let brd_lst = List.map (fun x -> create_t_brds board x "black") t_lst in
      let comb_lst = List.combine t_lst brd_lst in
      List.map (fun x -> create_nodes x 2 acc board) comb_lst)
  | _ -> acc

and create_nodes n depth acc board = 
  let scr = snd n in
  let coords = fst n in
  let x_init = coords |> fst |> fst  in
  let y_init = coords |> fst |> snd in
  let x_fin = coords |> snd |> fst in
  let y_fin = coords |> snd |> snd in
  if depth = 1 then 
    ( let tt_board = make_copy board in
      tt_board .(x_fin).(y_fin) <- tt_board.(x_init).(y_init);
      tt_board.(x_init).(y_init) <- Nil;
      let n1 = Node((scr,(x_init,y_init),(x_fin,y_fin)),(make_tree (depth+1) tt_board [])) in
      n1::acc;
      n1
    ) else (
    let n1 = Node((scr,(x_init,y_init),(x_fin,y_fin)),[]) in
    n1::acc;
    n1
  )

(**[check_lst_empty n] checks if [n] is empty or not.
   Requires:- A valid tree
   Returns:- a bool value *)
let check_lst_empty n = 
  match n with 
  | Node (_,l) -> if l = [] then true else false
  | _ -> false

(**[compare_nodes_min n1 n2] compares the scores of [n1] and [n2]
   Requires:- 2 valid trees
   Returns:- an int value *)
let compare_nodes_min n1 n2 = 
  match n1,n2 with 
  | Node ((scr1,(x1,y1),(x2,y2)),_), Node ((scr2,(x3,y3),(x4,y4)),_)-> compare scr1 scr2 
  | _ -> 0

(**[filter_spec_coords n1 t] checks if the end coordinates of [n1] are equal to [t]
   Requires:- A valid tree and coordinates
   Returns:- a bool value *)
let filter_sepc_coords n1 t = 
  match n1 with 
  | Node ((scr1,(x1,y1),(x2,y2)),_) -> if (x2 = fst t) && (y2 = snd t) then true else false
  | _ -> false

(** [get_min_val_coords lst cmpr_scr x1 y1 x2 y2] gives the coordinates of the node 
    with the minimum weight (that we got from the evaluation function of a board).
    Requires:- A valid tree list, an integer and 4 integer reprsenting coordinates
    Returns:- A tuple of coordinates*)
let rec get_minimum_value lst acc = 
  match lst with 
  | [] -> acc
  | Node((scr,(x1,y1),(x2,y2)),_)::t -> get_minimum_value t (if acc>= scr then scr else acc)
  | Root (_,_)::tl -> get_minimum_value tl acc

(** [update_node n] updates the score of [n] with the minimum value from the get_minimum_value
    function
    Requires:- A valid tree
    Returns:- A valid tree *)
let update_node n = 
  match n with 
  | Node ((scr,(x1,y1),(x2,y2)),l) -> (
      let min_val = get_minimum_value l 1000 in
      Node ((min_val,(x1,y1),(x2,y2)),l)
    )

(** [get_max_val_coords lst cmpr_scr x1 y1 x2 y2] gives the coordinates of the node 
    with the maximum weight (that we got from the evaluation function of a board).
    Requires:- A valid tree list, an integer and 4 integer reprsenting coordinates
    Returns:- A tuple of coordinates*)
let rec get_max_val_coords lst cmpr_scr x1 y1 x2 y2 = 
  match lst with 
  | [] -> ((x1,y1),(x2,y2))
  | Node ((scr,(m1,m2),(n1,n2)),_)::t -> (
      let t_scr = if cmpr_scr<= scr then scr else cmpr_scr in
      let q1 = if t_scr = scr then m1 else x1 in
      let q2 = if t_scr = scr then m2 else y1 in
      let w1 = if t_scr = scr then n1 else x2 in
      let w2 = if t_scr = scr then n2 else y2 in
      get_max_val_coords t t_scr q1 q2 w1 w2 
    )
  | Root(_,_)::t -> get_max_val_coords t cmpr_scr x1 y1 x2 y2


(** [get_best_move board] returns a string formatted according to the command type where
    it returns a string of the form "move beg-coords to end-coords". The end-coords are selected
    by getting the best possible move from a minimax tree of depth 2.
    Requires:- A valid 2d matrix
    Returns:- a valid string*)
let get_best_move board = 
  let tree_lst = make_tree 0 board [] in
  let rt = tree_lst |> List.hd in
  let d1_d2_lst = (match rt with 
      | Root (x,l) -> l
      | _ -> []) in
  let n_lst = List.map (fun x -> update_node x) d1_d2_lst in
  let tpl_coords = get_max_val_coords n_lst (-1000) 0 0 0 0 in
  let beg_coords = tpl_coords |> fst  in
  let end_coords = tpl_coords |> snd in
  let comm = "move " ^ (string_of_int (beg_coords|>fst)) ^ " " ^ (string_of_int (beg_coords|>snd)) ^ " to " ^ (string_of_int(end_coords|>fst)) ^ " " ^ (string_of_int(end_coords|>snd)) in
  comm

(** [check_winner] print outs the statement displaying the winner of the game or a tie
    based on the players' scores when the game times out *)
let check_winner () = 
  let white_string = string_of_int (!scoreWhite) in
  let black_string = string_of_int (!scoreBlack) in
  print_endline("\nWhite Score: " ^ white_string);
  print_endline ("Black Score: " ^ black_string);
  if !scoreWhite > !scoreBlack then Stdlib.print_string "\nTime's up! Player white has won!\n"
  else if !scoreWhite < !scoreBlack then Stdlib.print_string "\nTime's up!Player red has won!\n"
  else Stdlib.print_string "\nTime's up! It is a tie!\n"

(** [get_best_move_med board] returns a string formatted according to the command type where
    it returns a string of the form "move beg-coords to end-coords". The end-coords are selected
    by selecting at random from a list of possible moves.
    Requires:- A valid 2d matrix
    Returns:- a valid string*)
let get_best_move_med board = 
  let coords = move_med board "white" in
  let beg_coords = coords |> fst in
  let end_coords = coords|> snd in
  let comm = "move " ^ (string_of_int (beg_coords|>fst)) ^ " " ^ (string_of_int (beg_coords|>snd)) ^ " to " ^ (string_of_int(end_coords|>fst)) ^ " " ^ (string_of_int(end_coords|>snd)) in
  comm

(** [get_best_move_easy board] returns a string formatted according to the command type where
    it returns a string of the form "move beg-coords to end-coords". The end-coords are selected
    by selecting the first possible move from a list of possible moves.
    Requires:- A valid 2d matrix
    Returns:- a valid string*)
let get_best_move_easy board = 
  let coords = pos_moves_white board 7 7 [] |> List.hd in
  let beg_coords = coords |> fst in
  let end_coords = coords|> snd in
  let comm = "move " ^ (string_of_int (beg_coords|>fst)) ^ " " ^ (string_of_int (beg_coords|>snd)) ^ " to " ^ (string_of_int(end_coords|>fst)) ^ " " ^ (string_of_int(end_coords|>snd)) in
  comm

(** [execute_command board comm color] parses comm into a type Command, executes that 
    command onto the board, and returns a type result that corresponds to the
    new state of the board in a timed game.
    Requires: [board] is of type piece array array and [comm] is of type string
              [color] is of type string*)
let rec execute_command_time board comm color start_time length = 
  (* print_endline ("The unix time is " ^ string_of_float(Unix.time()));
     print_endline ("The total time taken should be " ^ string_of_float(start_time+.length)); *)
  if (Unix.time () -. start_time) > length  then (check_winner (); Stdlib.exit 0);
  match Command.parse comm with
  | Move ((x1, y1), (x2, y2)) -> move_board board x1 y1 x2 y2 color
  | Quit -> Quit_Game
  | Hint -> Hint_Game
  | Help -> Help_Game
  | EasyAI -> Illegal
  | MedAI -> Illegal
  | HardAI -> Illegal
  | exception Malformed -> Illegal
  | exception Empty -> Illegal 

(** [execute_command board comm color] parses comm into a type Command, executes that 
    command onto the board, and returns a type result that corresponds to the
    new state of the board.
    Requires: [board] is of type piece array array and [comm] is of type string
              [color] is of type string*)
let execute_command board comm color start_time  = 
  match Command.parse comm with
  | Move ((x1, y1), (x2, y2)) -> move_board board x1 y1 x2 y2 color
  | Quit -> Quit_Game
  | Hint -> Hint_Game
  | Help -> Help_Game
  | EasyAI -> Illegal
  | MedAI -> Illegal 
  | HardAI -> Illegal
  | exception Malformed -> Illegal
  | exception Empty -> Illegal


