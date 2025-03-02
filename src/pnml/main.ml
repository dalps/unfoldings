type place = { id : string; text : string; tokens : int }

type transition = { id : string; text : string }

type arc = { id : string; source : string; target : string }

type pnet = {
  id : string;
  places : place list;
  transitions : transition list;
  flow : arc list;
}

type want = pnet list

let pr = Printf.printf
let spr = Printf.sprintf

let id ic oc =
  let i = Xmlm.make_input (`Channel ic) in
  let o = Xmlm.make_output (`Channel oc) in
  let rec pull i o depth =
    Xmlm.output o (Xmlm.peek i);
    (* copy to output without consuming input *)
    match Xmlm.input i with
    (* consume input *)
    | `El_start _ -> pull i o (depth + 1)
    | `El_end ->
        if depth = 1 then (* we are exiting the root element *)
          ()
        else
          pull i o (depth - 1)
    | `Data _ -> pull i o depth
    | `Dtd _ -> assert false (* DTD not allowed here *)
  in
  (* the first line should be a Document Type Definition <!DOCTYPE ...> *)
  Xmlm.output o (Xmlm.input i);
  pull i o 0;
  if not (Xmlm.eoi i) then
    invalid_arg "document not well-formed"

let robot_model =
  "/home/dalpi/unfoldings/models/RobotManipulation/PT/robot-manipulation-1.pnml"

let robot_ic () = open_in robot_model

(* id (robot_ic ()) stdout *)

type tree = E of Xmlm.tag * tree list | D of string

let in_tree i =
  let el tag childs = E (tag, childs) in
  let data d = D d in
  Xmlm.input_doc_tree ~el ~data i

let out_tree o t =
  let fragment = function
    | E (tag, childs) -> `El (tag, childs)
    | D d -> `Data d
  in
  Xmlm.output_doc_tree fragment o t

(* let t = in_tree (Xmlm.make_input ~strip:true (`Channel (robot_ic ()))) *)

type 'a parse_result = ('a, string * Xmlm.pos) result

let ok a = Ok a
let error i = Error ("", Xmlm.pos i)
let error_msg msg i = Error (msg, Xmlm.pos i)

let accept s i =
  if Xmlm.input i = s then
    ok ()
  else
    error i

let want_tag name i : (string * string) list parse_result =
  match Xmlm.input i with
  | `El_start ((_, n), attr) when n = name ->
      pr "got %s\n" name;
      ok @@ List.map (fun ((_, name), value) -> (name, value)) attr
  | `El_start ((_, n), _) -> error_msg (spr "want: %s, got: %s" name n) i
  | _ -> error i

let end_tag i = accept `El_end i

(* list combinator for elements of the same type *)
let rec i_seq el acc i =
  let open CCResult in
  match Xmlm.peek i with
  | `El_start _ ->
      let* e = el i in
      i_seq el (e :: acc) i
  | `El_end -> ok (List.rev acc)
  | _ -> error_msg "bad sequence" i

let i_data n i =
  let open CCResult in
  let* _ = want_tag n i in
  let* d =
    match Xmlm.peek i with
    | `Data d ->
        ignore (Xmlm.input i);
        ok d
    | `El_end -> ok ""
    | _ -> error_msg "expected data, found element" i
  in
  let* _ = end_tag i in
  ok d

let i_el = i_data

let i_maybe el name i =
  let open CCResult in
  match Xmlm.peek i with
  | `El_start ((_, n), _) when n = name ->
      let* r = el n i in
      ok (Some r)
  | _ -> ok None

let in_pnet src : pnet list parse_result =
  let open CCResult in
  let i = Xmlm.make_input ~strip:true src in
  let _mk_tag n = (("", n), []) in
  (* check out the Xmlm.tag type *)
  let i_name i : string parse_result =
    let* _ = want_tag "name" i in
    let* text = i_data "text" i in
    let* _ = end_tag i in
    ok text
  in
  let i_place i : place parse_result =
    let* attrs = want_tag "place" i in
    let id = List.assoc "id" attrs in
    let* text = i_name i in
    pr "got place name: %s\n" text;
    let* tokens =
      match Xmlm.peek i with
      | `El_start ((_, "initialMarking"), _) ->
          (* wasted 15 min cuz I was matching the empty namespace *)
          let* _ = want_tag "initialMarking" i in
          pr "got marking";
          let* s = i_data "text" i in
          let* _ = end_tag i in
          ok (int_of_string s)
      | _ -> ok 0
    in
    let* _ = end_tag i in
    ok { id; text; tokens }
  in
  let i_transition i : transition parse_result =
    let* attrs = want_tag "transition" i in
    let id = List.assoc "id" attrs in
    let* text = i_name i in
    let* _ = end_tag i in
    ok { id; text }
  in
  let i_arc i : arc parse_result =
    let* attrs = want_tag "arc" i in
    let id = List.assoc "id" attrs in
    let source = List.assoc "source" attrs in
    let target = List.assoc "target" attrs in
    let* _ = end_tag i in
    ok { id; source; target }
  in
  let i_page_item i =
    match Xmlm.peek i with
    | `El_start ((_, "place"), _) ->
        let* p = i_place i in
        ok @@ `place p
    | `El_start ((_, "transition"), _) ->
        let* t = i_transition i in
        ok @@ `trans t
    | `El_start ((_, "arc"), _) ->
        let* a = i_arc i in
        ok @@ `arc a
    | _ -> error i
  in
  let i_page i : (place list * transition list * arc list) parse_result =
    let* _ = want_tag "page" i in
    (* we can ignore the id *)
    let* _ = i_name i in
    pr "ignoring page name\n";
    let* items = i_seq i_page_item [] i in
    let* _ = end_tag i in
    let ps, mixed =
      List.partition_map
        (function
          | `place p -> Left p
          | `trans t -> Right (`trans t)
          | `arc a -> Right (`arc a))
        items
    in
    let ts, arcs =
      List.partition_map
        (function
          | `trans t -> Left t
          | `arc a -> Right a)
        mixed
    in
    ok (ps, ts, arcs)
  in
  let i_net i : pnet parse_result =
    let* attrs = want_tag "net" i in
    CCList.pp (CCPair.pp CCString.pp CCString.pp) Format.std_formatter attrs;
    let id = List.assoc "id" attrs in
    pr "ID: %s\n" id;
    let* places, transitions, flow = i_page i in
    (* handle other elements *)
    let* _ = i_name i in
    let* _ = end_tag i in
    ok { id; places; transitions; flow }
  in
  let* _ = accept (`Dtd None) i in
  let* _ = want_tag "pnml" i in
  let* nets = i_seq i_net [] i in
  let* _ = end_tag i in
  if not (Xmlm.eoi i) then
    error i
  else
    ok nets

let nets = in_pnet (`Channel (robot_ic ()))
