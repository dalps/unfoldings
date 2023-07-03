open Unfold
open Branching_process
open Prettyprint

module RepeatedExecSS : SearchScheme = struct
  let r_occurrences (e : Event.t) goals =
    (* List.length (List.filter (fun t -> List.mem t goals) e.history) *)
    List.fold_left
      (fun acc t -> if List.mem t goals then acc+1 else acc)
      0
      e.history

  let is_terminal (e : Event.t) n stgy goals =
    let b = EventSet.exists
      (fun e' -> stgy e'.history e.history < 0 && StateSet.equal
        (labels_of_places (postset_t n e'))
        (labels_of_places (postset_t n e)) &&
        ((is_predecessor (Node.of_trans e') (Node.of_trans e) n) ||
        r_occurrences e' goals >= r_occurrences e goals))
      (transitions n)
    in
    print_endline ("Is " ^ string_of_trans (Event.label e) ^ " terminal? -> " ^ string_of_bool b);
    b

  let is_successful (e : Event.t) n stgy goals =
    let b = EventSet.exists
      (fun e' -> stgy e'.history e.history < 0 && StateSet.equal
        (labels_of_places (postset_t n e'))
        (labels_of_places (postset_t n e)) &&
        (is_predecessor (Node.of_trans e') (Node.of_trans e) n) &&
        let _ = print_string (string_of_trans (Event.label e) ^ " is of type (a)") in
        let b = r_occurrences e' goals < r_occurrences e goals in
        if b then (print_endline (", also #R(e') = " ^ string_of_int (r_occurrences e' goals) ^ " < " ^ string_of_int (r_occurrences e goals) ^ " = #R(e)"); b) else b)
      (transitions n)
    in
    print_endline ("Is " ^ string_of_trans (Event.label e) ^ " successful? -> " ^ string_of_bool b);
    b
end

include Unfold(RepeatedExecSS)