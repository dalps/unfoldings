open Unfold
open Occurrence_net

module RepeatedExecSS : SearchScheme = struct
  let r_occurrences (e : Event.t) goals =
    List.length (List.filter (fun t -> List.mem t goals) (Event.history e))

  let is_terminal (e : Event.t) n stgy goals =
    TransSet.exists
      (fun e' ->
        stgy (Event.history e') (Event.history e) < 0
        && Product.PlaceSet.equal
             (labels_of_places (postset_t n e'))
             (labels_of_places (postset_t n e))
        && (is_predecessor (Node.of_trans e') (Node.of_trans e) n
           || r_occurrences e' goals >= r_occurrences e goals))
      (transitions n)

  let is_successful (e : Event.t) n stgy goals =
    TransSet.exists
      (fun e' ->
        stgy (Event.history e') (Event.history e) < 0
        && Product.PlaceSet.equal
             (labels_of_places (postset_t n e'))
             (labels_of_places (postset_t n e))
        && is_predecessor (Node.of_trans e') (Node.of_trans e) n
        && r_occurrences e' goals < r_occurrences e goals)
      (transitions n)
end

include Unfold (RepeatedExecSS)
