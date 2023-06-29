open Unfold
open Branching_process

module RepeatedExecSS : SearchScheme = struct
  let r_occurrences (e : Event.t) goals =
    (* List.length (List.filter (fun t -> List.mem t goals) e.history) *)
    List.fold_left
      (fun acc t -> if List.mem t goals then acc+1 else acc)
      0
      e.history

  let is_terminal (e : Event.t) n stgy goals =
    EventSet.exists
      (fun e' -> stgy e'.history e.history < 0 && StateSet.equal
        (labels_of_places (outputs_of_trans e' n))
        (labels_of_places (outputs_of_trans e n)) &&
        ((is_predecessor (Node.of_trans e') (Node.of_trans e) n) ||
        r_occurrences e' goals >= r_occurrences e goals))
      (transitions n)

  let is_successful (e : Event.t) n stgy goals =
    EventSet.exists
      (fun e' -> stgy e'.history e.history < 0 && StateSet.equal
        (labels_of_places (outputs_of_trans e' n))
        (labels_of_places (outputs_of_trans e n)) &&
        (is_predecessor (Node.of_trans e') (Node.of_trans e) n) &&
        r_occurrences e' goals < r_occurrences e goals)
      (transitions n)
end

include Unfold(RepeatedExecSS)