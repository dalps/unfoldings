open Unfold
open Branching_process

module ExecutabilitySS : SearchScheme = struct
  let is_terminal e n stgy =
    EventSet.exists
      (fun e' -> stgy (past_word e' n) (past_word e n) < 0 && StateSet.equal
        (labels_of_places (Branching_process.outputs_of_trans e' n)) 
        (labels_of_places (Branching_process.outputs_of_trans e n)))
      (Branching_process.transitions n)

  let is_successful e _ goals =
    List.mem (Event.label e) goals
end

include Unfold(ExecutabilitySS)