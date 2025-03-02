open Main

let model = "/home/dalpi/unfoldings/models/bureaucrats.xml"

type bureaucrat = {
  name : string;
  surname : string;
  honest : bool;
  obfuscation_level : float;
  trs : string list;
}

(** Deserialize a list of bureaucrats *)
let in_w3c_bureaucrats src : bureaucrat list parse_result =
  let open CCResult in
  let i = Xmlm.make_input ~strip:true (`Channel (open_in src)) in
  let tag n = (("", n), []) in
  let i_bureaucrat i =
    let* _ = accept (`El_start (tag "bureaucrat")) i in
    let* name = i_el "name" i in
    let* surname = i_el "surname" i in
    let* honest =
      match Xmlm.peek i with
      | `El_start (("", "honest"), []) ->
          let* _ = i_el "honest" i in
          ok true
      | _ -> ok false
    in
    let* obf = i_el "obfuscation_level" i in
    let obfuscation_level = float_of_string obf in
    let* trs = i_seq (i_el "tr") [] i in
    let* _ = end_tag i in
    ok { name; surname; honest; obfuscation_level; trs }
  in
  let* _ = accept (`Dtd None) i in
  let* _ = want_tag "list" i in
  print_endline "got list";
  let* bl = i_seq i_bureaucrat [] i in
  let* _ = end_tag i in
  if not (Xmlm.eoi i) then
    error i
  else
    ok bl
