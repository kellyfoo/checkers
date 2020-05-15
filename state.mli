(** [result] is a representation of the game after a command is executed *)
type result =
  | State of (Static.piece array array)
  | Timed of (Static.piece array array)
  | Hint_Game
  | Help_Game
  | Quit_Game
  | Illegal

(** [tree] is a representation of the possible outcomes used to determine
    the best move of the hard AI *)
type tree = Root of int * tree list | Node of (int * (int *int) * (int*int)) * tree list

(** [make_copy brd] returns a copy of the given board *)
val make_copy: Static.piece array array -> Static.piece array array

(** [check_piece brd x y] returns the string representation of the type of piece 
    at (x,y) on the given board *)
val check_piece: Static.piece array array -> int -> int -> string

(** [check_piece_piece p] returns the string representation of the type of piece given  *)
val check_piece_piece: Static.piece -> string

(** [remove_piece brd x y] removes the piece at (x,y) of the given board *)
val remove_piece: Static.piece array array -> int -> int -> unit

(** [can_jump_to brd x y] returns whether it's possible for a piece to jump to (x,y) *)
val can_jump_to: Static.piece array array -> int -> int -> bool

(** [valid_jump brd x1 y1 x2 y2] returns whether it's possible for the piece at 
    (x1,y1) to (x2,y2) *)
val valid_jump: Static.piece array array -> int -> int -> int -> int -> result

(** [is_jump x1 y1 x2 y2] returns whether the move from (x1,y1) to (x2,y2) is a jump *)
val is_jump: int -> int -> int -> int -> bool

(** [move_standard brd x1 y1 x2 y2] returns the result of moving a piece from (x1,y1)
    to (x2,y2) *)
val move_standard: Static.piece array array -> int -> int -> int -> int -> result

(** [valid_move brd x1 y1 x2 y2] returns if a move from (x1,y1) to (x2,y2) is valid *)
val valid_move: Static.piece array array -> int -> int -> int -> int -> result


(** [execute_command_time brd com col st len] returns the result of executing the 
    string representation of a command under a time restriction *)
val execute_command_time : Static.piece array array -> string -> string -> float -> float -> result

(** [execute_command_time brd com col st] returns the result of executing a string
    representation of a command *)
val execute_command : Static.piece array array -> string -> string -> float -> result


(** [get_best_move brd] returns a string representation of the command to execute
    the best move which uses the hard AI *)
val get_best_move : Static.piece array array -> string

(** [get_best_move_med brd] returns a string representation of the command to execute
    the best move which uses the medium AI *)
val get_best_move_med : Static.piece array array -> string

(** [get_best_move_easy brd] returns a string representation of the command to execute
    the best move which uses the easy AI *)
val get_best_move_easy : Static.piece array array -> string


(** [pos_moves_piece_black brd r c acc] returns the list of all possible moves for player black 
    of the specified piece (in the form of the start & end coordinate) *)
val pos_moves_piece_black: Static.piece array array -> int -> int -> ((int * int) * (int * int)) list -> ((int * int) * (int * int)) list

(** [pos_moves_white brd r c acc] returns the list of all possible moves for player white
    (in the form of the start & end coordinate) *)
val pos_moves_white: Static.piece array array -> int -> int -> ((int * int) * (int * int)) list -> ((int * int) * (int * int)) list

(** [pos_moves_black brd r c acc] returns the list of all possible moves for player black
    (in the form of the start & end coordinate) *)
val pos_moves_black: Static.piece array array -> int -> int -> ((int * int) * (int * int)) list -> ((int * int) * (int * int)) list

(** [check_lst_empty t] returns if the given tree is empty*)
val check_lst_empty : tree -> bool