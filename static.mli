(** [color] represents the color of a piece *)
type color = Black | White

(** [coordinates] is a [int*int] representation of the position of the piece. *)
type coordinates = None | Some of (int * int)

(** [piece] is the representation of the moving pieces on the board *)
type piece =
  | Normal of (color * coordinates * int)
  | King of (color * coordinates * int)
  | Pos
  | Nil

(** [create_board r c] creates an empty board with r rows and c columns *)
val create_board : int -> int -> piece array array

(** [create_board_init_h r c brd] places checkers at their initial position *)
val create_board_init_h : int -> int -> piece array array -> unit

(** [pp_arr r c brd] prints the board in a user-friendly manner *)
val pp_arr : int -> int -> piece array array -> unit 

(** [print_line x] prints a dashed line to define the rows of the board *)
val print_line : int -> unit

(** [print_piece p] prints the image associated with a certain piece *)
val print_piece : piece -> unit