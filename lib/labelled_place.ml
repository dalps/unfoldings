type t = {
  name : int;
  label : string
}

let build name lbl = {name = name; label = lbl}

let name p = p.name

let label p = p.label
  
let compare p1 p2 = compare p1.name p2.name