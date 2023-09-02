(* GNBA of Fig. 4.19 p. 194 (Baier-Katoen) *)
module CritAP = struct
  type t = Crit1 | Crit2

  let compare = compare
end

module CritLTL = Unfoldings.Ltl.Make (CritAP)
module CritGNBA = Unfoldings.Gnba.Make (String) (CritLTL.Formula)
open CritGNBA

let g =
  CritLTL.Formula.(
    CritGNBA.of_lists [ "q0"; "q1"; "q2" ] [ AP Crit1; AP Crit2 ]
      [
        delta "q1" True [ "q0" ];
        delta "q0" (AP Crit1) [ "q1" ];
        delta "q0" (AP Crit2) [ "q2" ];
        delta "q0" True [ "q0" ];
        delta "q2" True [ "q0" ];
      ]
      [ "q0" ] [ [ "q1" ]; [ "q2" ] ])
