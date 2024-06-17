%parameter<Semantics : Parser_sig.S>

%{
  open Semantics
%}

%start <[ `N of N.t | `P of P.t ] list> main

%%

let main := ~ = list(declaration); EOF; <>

let declaration :=
| ~ = petrinet_declaration; <`N> 
| ~ = product_declaration; <`P>

let product_declaration :=
  "product"; "{";
    nets = list(petrinet_declaration); 
  "}"; "{";
    sync = separated_list(";", global_transition);
  "}"; {
    P.product nets sync
  }

let global_transition :=
  ~ = separated_nonempty_list(",", idle_or_transition); <>

let idle_or_transition :=
| "idle"; { `Idle }
| ~ = local_transition; <`T>

let petrinet_declaration :=
| "{"; ~ = petrinet_declaration; "}"; <>
| "places"; ":"; places = place_list; ";";
  "trans"; ":"; transitions = local_transition_list; ";";
  arcs = arc_list;
  "tokens"; ":"; tokens = place_list; {
    N.of_lists places transitions arcs tokens
  }

let arc_list := ~ = list(arc); <>

let arc :=
  transition = local_transition; ":"; preset = place_list; "-->"; postset = place_list; ";"; {
    (N.( --> ) preset postset transition)
  }

let local_transition_list := ~ = separated_list(",", local_transition); <>
let place_list := ~ = separated_list(",", place); <>

let local_transition := id = "x"; { inject_trans id }
let place := id = "x"; { inject_place id }