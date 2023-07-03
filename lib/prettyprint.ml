open Occurrence_net
open Product.GlobalT

let string_of_local_t = function Idle -> "_" | T s -> s

let rec string_of_trans = function
  | [] -> ""
  | [ t ] -> string_of_local_t t
  | t :: ls -> string_of_local_t t ^ "," ^ string_of_trans ls

let rec string_of_history = function
  | [] -> ""
  | [ t ] -> string_of_trans t
  | t :: ts -> string_of_trans t ^ "; " ^ string_of_history ts

let string_of_event (e : Event.t) =
  "{name = "
  ^ string_of_int (Event.name e)
  ^ "; history = ["
  ^ string_of_history (Event.history e)
  ^ "]; label = \""
  ^ string_of_trans (Event.label e)
  ^ "\"}"

let string_of_place (p : Token.t) =
  "{history = ["
  ^ string_of_history (Token.history p)
  ^ "]; label = \"" ^ Token.label p ^ "\"}"

let string_of_placeset ps =
  "["
  ^ PlaceSet.fold
      (fun (p : Token.t) acc -> acc ^ string_of_place p ^ "; ")
      ps ""
  ^ "]"

let string_of_eventset es =
  "["
  ^ TransSet.fold
      (fun (e : Event.t) acc -> acc ^ string_of_event e ^ "; ")
      es ""
  ^ "]"
