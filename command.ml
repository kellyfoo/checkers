type locations = (int * int) * (int * int)

type command = 
  | Move of locations
  | Quit 
  | Hint
  | Help 
  | EasyAI
  | MedAI
  | HardAI

exception Empty

exception Malformed


(** [has_move str] returns true if [str] beings with the verb "move" and false
    otherwise 
    Requires: [str] is a valid string representation *)
let has_move str = 
  if String.get str 0 = 'm' && String.get str 1 = 'o' && String.get str 2 = 'v' && String.get str 3 = 'e' then true
  else false

(** [has_quit str] returns true if [str] beings with the verb "quit" and false
    otherwise 
    Requires: [str] is a valid string representation *)
let has_quit str = 
  if String.get str 0 = 'q' && String.get str 1 = 'u' && String.get str 2 = 'i' && String.get str 3 = 't' then true
  else false

let has_hint str = 
  if String.get str 0 = 'h' && String.get str 1 = 'i' && String.get str 2 = 'n' && String.get str 3 = 't' then true
  else false

let has_help str = 
  if String.get str 0 = 'h' && String.get str 1 = 'e' && String.get str 2 = 'l' && String.get str 3 = 'p' then true
  else false


let has_easy_ai str = 
  if String.get str 0 = 'e' && String.get str 1 = 'a' && String.get str 2 = 'i' then true
  else false

let has_med_ai str = 
  if String.get str 0 = 'm' && String.get str 1 = 'a' && String.get str 2 = 'i' then true
  else false

let has_hard_ai str = 
  if String.get str 0 = 'h' && String.get str 1 = 'a' && String.get str 2 = 'i' then true
  else false

(** [find_locations str] returns a tuple with the start coordinate as the first element of the tuple
    and the end coordinate as the second element of the tuple.
    Requires: [str] is a valid string representation *)
let find_locations str = 
  let after_move = String.sub str 5 (String.length str - 5) in 
  let start_x = Char.code (String.get after_move 0) - 48 in 
  let start_y = Char.code (String.get after_move 2) - 48 in
  let end_x = Char.code (String.get after_move 7) - 48 in
  let end_y = Char.code (String.get after_move 9) - 48 in 
  ((start_x, start_y), (end_x, end_y))

(** [list_to_string lst] returns a string with all the strings in [lst] concatenated into one string. 
    Requires: [lst] is a valid list of type strings *)
let rec list_to_string lst = 
  match lst with 
  | [] -> ""
  | h :: t -> h ^ list_to_string t

(** [matches_move str] returns true if [str] is a properly formed move command and false otherwise.
    Requires: [str] is a valid string representation. *)
let matches_move str = 
  if String.trim (String.sub str 4 (String.length str - 4)) = "" then false 
  else let after_move = String.sub str 5 (String.length str - 5) in 
    let split_str = String.split_on_char ' ' after_move in
    if Str.string_match (Str.regexp "[0-7][0-7][t][o][0-7][0-7]") (list_to_string split_str) 0 then true
    else false


(** [matches_quit str] returns true if [str] is a properly formed quit command and false otherwise.
    Requires: [str] is a valid string representation. *)
let matches_quit str = 
  if String.trim (String.sub str 4 (String.length str - 4)) = "" then true
  else false

let matches_hint str = 
  if String.trim (String.sub str 4 (String.length str - 4)) = "" then true
  else false

let matches_help str = 
  if String.trim (String.sub str 4 (String.length str - 4)) = "" then true
  else false

let matches_easy_ai str = 
  if String.trim (String.sub str 3 (String.length str - 3)) = "" then true
  else false

let matches_med_ai str = 
  if String.trim (String.sub str 3 (String.length str - 3)) = "" then true
  else false

let matches_hard_ai str = 
  if String.trim (String.sub str 3 (String.length str - 3)) = "" then true
  else false

(** [find_command str] returns Move command with locations, Quit command, or Start command depending 
    on the command given in [str]. 
    [Malformed] is raised if the command given by [str] is not a valid command.
    Requires: [str] is a valid string representation. *)
let find_command str = 
  if has_move str then Move (find_locations str)
  else if has_quit str then Quit
  else if has_hint str then Hint
  else if has_help str then Help
  else if has_easy_ai str then EasyAI
  else if has_med_ai str then MedAI
  else if has_hard_ai str then HardAI
  else raise Malformed

(** [parse str] processes the command given in [str]. 
    [Empty] is raised if [str] does not contain any command. 
    [Malformed] is raised if [str] does not appropriately represent a command.
    Requires: [str] is a valid string representation. *)
let parse str = 
  if str = "" then raise Empty
  else if String.trim str = "" then raise Empty
  else if has_move str && not (matches_move str) then raise Malformed
  else if has_quit str && not (matches_quit str) then raise Malformed
  else if has_hint str && not (matches_hint str ) then raise Malformed
  else if has_help str && not (matches_help str) then raise Malformed
  else if has_easy_ai str && not (matches_easy_ai str) then raise Malformed
  else if has_med_ai str && not (matches_med_ai str) then raise Malformed
  else if has_hard_ai str && not (matches_hard_ai str) then raise Malformed
  else find_command str 