open Branching_process
open Product.Trans

let string_of_local_t = function
  | Idle -> "_"
  | T s -> s
  | U s -> "!" ^ s

let rec string_of_trans = function
  | [] -> ""
  | [t] -> string_of_local_t t
  | t::ls ->  string_of_local_t t ^ "," ^ string_of_trans ls

let rec string_of_history = function
  | [] -> ""
  | [t] -> string_of_trans t
  | t::ts -> "\"" ^ string_of_trans t ^ "\"; " ^ string_of_history ts

let string_of_event (e : Event.t) = 
  "{name = " ^ string_of_int e.name ^ "; 
    history = [" ^ string_of_history e.history ^ "]; 
    label = \"" ^ string_of_trans e.label ^ "\"}"

let string_of_place (p : LabelledPlace.t) = 
  "{history = [" ^ string_of_history p.history ^ "]; 
    label = \"" ^ p.label ^ "\"}"

let string_of_placeset ps =
  "[" ^
  (PlaceSet.fold
    (fun (p : LabelledPlace.t) acc -> acc ^ string_of_place p ^ "; ") ps "")
  ^ "]"

let string_of_eventset es =
  "[" ^
  (Branching_process.TransSet.fold
    (fun (e : Event.t) acc -> acc ^ string_of_event e ^ "; ") es "")
  ^ "]"
