open Base
open Caml.Format
open Utils

type variants =
  | LetExp
  | IfThenElse
  | Match

type statistics = {
  case : variants;
  mutable count : int
  };;

let countOfIfThen = ref 0
let countOfMatch = ref 0
let countOfLet = ref 0

(** Update statistics. *)
let add = function
  | LetExp -> countOfLet := !countOfLet + 1
  | IfThenElse -> countOfIfThen := !countOfIfThen + 1
  | Match -> countOfMatch := ! countOfMatch + 1

let report () = 
  Caml.Format.printf "CountOfThen: %d\n" !countOfIfThen;
  Caml.Format.printf "Count Of Match: %d\n" !countOfMatch;
  Caml.Format.printf "CountOfLet: %d\n" !countOfLet
;;
