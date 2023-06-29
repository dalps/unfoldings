open Unfold
open Branching_process

module ExecutabilitySS : SearchScheme = struct
  let is_terminal (e : Event.t) n stgy _ =
    EventSet.exists
      (fun e' -> stgy e'.history e.history < 0 && StateSet.equal
        (labels_of_places (outputs_of_trans e' n)) 
        (labels_of_places (outputs_of_trans e n)))
      (transitions n)

  let is_successful e _ _ goals =
    List.mem (Event.label e) goals
end

include Unfold(ExecutabilitySS)