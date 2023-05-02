# unfoldings
This project contains simple representations of Petri nets and other useful structures for the purpose of model checking.

## Defining a Petri net
For the sake of simplicity, places and transitions are represented as strings.

Start by opening the `Petrinet` module.
The `build` function of the module allows the definition of a new net by supplying as arguments:
* a list of string-encoded places, e.g. `["p1","p2"]`. This is the set of places of the net.
* a list of string-encoded transitions, e.g. `["t1","t2"]`. This it the set of transitions of the net.
* a list of arcs. This is the flow relation of the net.

  Arcs can be easily represented as follows:
  + `"p1" @--> "t1"` is the arc connecting place `p1` to transition `t1` 
  + `"t1" -->@ "p2"` is the arc connecting transition `t1` to place `p2`
  
  Notice the different semantics of the `@-->` and `-->@` infix operators provided by the module.
* a list of string-encoded places, e.g. `["p1"]`. This is the _initial marking_ of the net.

Here's a full-fledged definition:
```
let n = Petrinet.build
  ["p1","p2"]
  ["t1","t2"]
  [ "p1" @--> "t1"; 
    "t1" -->@ "p2";
    "p2" @--> "t2";
    "t2" -->@ "p1" ]
  ["p1"]
;;
```
See more in [examples](/examples).
