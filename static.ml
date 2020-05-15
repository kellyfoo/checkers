open Array

(** [color] is the color of the piece on the board *)
type color = Black | White

(** [coordinates] is a [(int*int)] represenattion of the position of the piece. *)
type coordinates = None | Some of (int * int)

(** [piece] is the representation of the moving pieces on the board *)
type piece =
  | Normal of (color * coordinates * int)
  | King of (color * coordinates * int)
  | Pos
  | Nil


(** [create_board] creates an empty board of size [rows] * [cols]*)
let create_board(rows:int)(cols:int) = 
  Array.make_matrix rows cols Nil

(** [update_arr cell] adds a [piece] of [color] on the board [arr] with 
    coordinates [(row_num),(col_num)] *)
let update_arr_cell_norm (row_num)(col_num)(arr)(color) = 
  arr.(row_num).(col_num) <- Normal (color, Some (row_num,col_num),3)

(** [update_row_even] parses through all the even columns that is column 2,4..8 and 
    adds pieces to the interesection of every even column with [row_num]*)
let rec update_row_even (row_num)(cols)(color)(arr) = 
  (* print_endline("this is function even ");
     print_endline ("number of columns are " ^ string_of_int (cols)); *)
  if cols >= 2 && cols <= 7 then
    (update_arr_cell_norm(row_num)(cols)(arr)(color);
     update_row_even (row_num)(cols-2)(color)(arr))
  else
  if cols = 0 then 
    update_arr_cell_norm(row_num)(cols)(arr)(color)
  else 
    failwith "not appropriate number of columns"

(** [same as update_row_even but for odd columns]*)
let rec update_row_odd (row_num)(cols)(color)(arr) = 
  if cols >= 2 && cols <= 7 then
    (update_arr_cell_norm(row_num)(cols)(arr)(color);
     update_row_odd (row_num)(cols-2)(color)(arr))
  else
  if cols = 1 then 
    update_arr_cell_norm(row_num)(cols)(arr)(color)
  else 
    failwith "not appropriate number of columns"

(** creates a basic board of white and black pieces on the checker board*)
let rec create_board_init_h (rows)(cols)(arr) = 
  let row_num = (rows-1) in
  if row_num = 7 || row_num = 5 then
    (update_row_even row_num (cols-2) (White) arr;
     create_board_init_h (rows-1)(cols)(arr))
  else
  if row_num = 6  then
    (update_row_odd row_num (cols-1) (White) arr;
     create_board_init_h (rows-1)(cols)(arr))
  else
  if row_num = 2 then 
    (update_row_odd row_num (cols-1) (Black) arr;
     create_board_init_h (rows-1)(cols)(arr))
  else 
  if row_num = 1 then
    (update_row_even row_num (cols-2) (Black) arr;
     create_board_init_h (rows-1)(cols)(arr))
  else
  if row_num = 0 then
    update_row_odd row_num (cols-1) (Black) arr
  else
    create_board_init_h (rows-1)(cols)(arr)


(** [print_line n] prints n dashes in a line *)
let rec print_line n = 
  match n with
  | 0 -> print_string "-"
  | x -> print_string "-"; print_line (n-1) 

(* let if_cols_even col = 
   if col mod 2 = 0 then true else false *)

let print_piece (t) = 
  match t with
  | Normal (Black, Some(x,y),3) -> ANSITerminal.(print_string [white] " 🔴 ")
  | Normal (White, Some(x,y),3) -> ANSITerminal.(print_string [white]" ⚪ ")
  | King (Black, Some(x,y),5) -> ANSITerminal.(print_string [white]" 🅾️  ")
  | King (White, Some(x,y),5)-> ANSITerminal.(print_string [white]" 0️⃣  ")
  | Pos -> ANSITerminal.(print_string [white]" ✅ ")
  | Nil-> ANSITerminal.(print_string [white] " X  ")
  | _ -> print_string "| X "

let rec print_row (row)(cols)(arr:piece array array) = 
  if cols>= 0 && cols<=6 then
    let t_piece = arr.(row).(cols) in
    (
      print_piece (t_piece); print_string "|";
      print_row (row)(cols+1)(arr))
  else
  if cols = 7 then
    let t_piece = arr.(row).(cols) in
    print_piece(t_piece);print_string "|";
  else
    failwith "there is a column problem"


let rec pp_arr (rows)(cols)(arr:piece array array) = 
  if rows >= 0 && rows<=6 then
    (  ANSITerminal.(print_string [green] ( "  " ^ string_of_int rows ^ " " ));
       print_string "|";
       print_row (rows)(cols)(arr);
       print_endline "   ";
       print_string "    ";
       print_line (40);
       print_endline "";
       pp_arr(rows+1)(cols)(arr))
  else
  if rows = 7 then
    ( ANSITerminal.(print_string [green] ( "  " ^ string_of_int rows ^ " " ));
      print_string "|";
      print_row(rows)(cols)(arr);
      print_endline "";
      print_string "    ";
      print_line(40);
      print_endline "";)
  else
    failwith "There is a row problem"