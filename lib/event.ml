type t = {
  name : int;
  label : Product_transition.t
}

let build name lbl = {name = name; label = lbl}

let name e = e.name

let label e = e.label
  
let compare = compare