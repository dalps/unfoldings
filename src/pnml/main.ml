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

let robot_ic () = open_in robot_model;;

id (robot_ic ()) stdout

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

let t = in_tree (Xmlm.make_input ~strip:true (`Channel (robot_ic ())))

let in_pnet src : pnet list =
  let i = Xmlm.make_input ~strip:true src in
  let _mk_tag n = (("", n), []) in
  (* check out the Xmlm.tag type *)
  let error () = invalid_arg "parse error" in
  let accept s i =
    if Xmlm.input i = s then
      ()
    else
      error ()
  in
  let want_tag name i : (string * string) list =
    match Xmlm.input i with
    | `El_start ((_, n), attr) when n = name ->
        List.map (fun ((_, name), value) -> (name, value)) attr
    | _ -> error ()
  in
  let end_tag () = accept `El_end i in
  (* list combinator for elements of the same type *)
  let rec i_seq el acc i =
    match Xmlm.peek i with
    | `El_start _ -> i_seq el (el i :: acc) i
    | `El_end -> List.rev acc
    | _ -> error ()
  in
  let i_data n i =
    ignore (want_tag n i);
    let d =
      match Xmlm.peek i with
      | `Data d ->
          ignore (Xmlm.input i);
          d
      | `El_end -> ""
      | _ -> error ()
    in
    end_tag ();
    d
  in
  let i_place i : place =
    let attrs = want_tag "place" i in
    let id = List.assq "id" attrs in
    want_tag "name" i |> ignore;
    let text = i_data "text" i in
    end_tag ();
    end_tag ();
    let tokens = 0 in
    { id; text; tokens }
  in
  let i_transition i : transition =
    let attrs = want_tag "transition" i in
    let id = List.assq "id" attrs in
    want_tag "name" i |> ignore;
    let text = i_data "text" i in
    end_tag ();
    end_tag ();
    { id; text }
  in
  let i_arc i : arc =
    let attrs = want_tag "arc" i in
    let id = List.assq "id" attrs in
    let source = List.assq "source" attrs in
    let target = List.assq "target" attrs in
    end_tag ();
    { id; source; target }
  in
  let i_page_item i =
    match Xmlm.peek i with
    | `El_start ((_, "place"), _) -> `place (i_place i)
    | `El_start ((_, "transition"), _) -> `trans (i_transition i)
    | `El_start ((_, "arc"), _) -> `arc (i_arc i)
    | _ -> error ()
  in
  let i_page i : place list * transition list * arc list =
    want_tag "page" i |> ignore;
    (* we can ignore the id *)
    let items = i_seq i_page_item [] i in
    end_tag ();
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
    (ps, ts, arcs)
  in
  let i_net i : pnet =
    let attrs = want_tag "net" i in
    let id = List.assq "id" attrs in
    let places, transitions, flow = i_page i in
    (* handle other elements *)
    end_tag ();
    { id; places; transitions; flow }
  in
  let i_pnml i =
    want_tag "pnml" i |> ignore;
    let nets = i_seq i_net [] i in
    end_tag ();
    nets
  in
  i_pnml i
