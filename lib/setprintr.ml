module Make (Set : Set.S) = struct
  let string_of_t string_of_elt s =
    let l = Set.elements s in
    match l with
    | [] -> "∅"
    | _ -> "{" ^ String.concat ", " (List.map string_of_elt l) ^ "}"

  let print_set string_of_elt s = print_endline (string_of_t string_of_elt s)
end

let string_of_set (type s t)
    (module Set : Set.S with type elt = s and type t = t) string_of_elt set =
  let l = Set.elements set in
  match l with
  | [] -> "∅"
  | _ -> "{" ^ String.concat ", " (List.map string_of_elt l) ^ "}"
