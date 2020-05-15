(** [locations] is an [[int*int] * [int*int]] representation of two different
    locations of the board *)
type locations = (int * int) * (int * int)

(** [command] is a representation of the type of command given by a user *)
type command = 
  | Move of locations
  | Quit 
  | Hint
  | Help
  | EasyAI
  | MedAI
  | HardAI

(** [Empty] represents the exception caused by an empty command *)
exception Empty

(** [Malformed] represents the exception caused by a malformed command *)
exception Malformed

(** [parse s] represents the given string as a type command, or raises an 
    appropriate exception *)
val parse : string -> command