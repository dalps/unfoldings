type t = {
  name : int;
  label : string
}

let build name lbl = {name = name; label = lbl}

let build_anon lbl = {name = 0; label = lbl} 

let name_of e = e.name

let label_of e = e.label
  
let compare = compare