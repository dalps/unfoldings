type t = {
  name : int;
  label : State.t
}

let build name lbl = {name = name; label = lbl}

let name p = p.name

let label p = p.label
  
let compare p1 p2 = p1.name - p2.name