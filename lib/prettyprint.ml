open Branching_process

let print_history h =
  List.iter (fun t -> print_string ("\"" ^ Product_transition.string_of_t t ^ "\"; ")) h

let print_placeset ps =
  print_string "[";
  List.iter (fun (p : Labelled_place.t) -> print_string "{history = ["; 
    print_history p.history;
    print_string ("]; label = \"" ^ p.label ^ "\"}; ")) 
  (PlaceSet.elements ps);
  print_endline "]\n"

let print_eventset es =
  print_string "[";
  List.iter (fun (e : Event.t) -> print_string "{history = ["; 
    print_history e.history;
    print_string ("]; label = \"" ^ Product_transition.string_of_t e.label ^ "\"}; ")) 
  (TransSet.elements es);
  print_endline "]\n"