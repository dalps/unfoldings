open Petrilib
open Unfoldlib
module StringPTNet = Petrinet.Make (String) (String)
module StringPTNetProduct = Product.Make (StringPTNet)
module StringProductUnfolder = Executability.Make (StringPTNetProduct)
open StringProductUnfolder
open Product
open Utils

type semantics = N of StringPTNet.t | P of StringPTNetProduct.t

let string_of_globaltrans t =
  let n = List.length t in
  spr "%s%s%s"
    (if n > 1 then "(" else "")
    (String.concat ","
       (List.fold_right
          (fun lt ->
            List.cons
              (match lt with
              | `T s -> s
              | `Idle -> "ϵ"))
          t []))
    (if n > 1 then ")" else "")

let string_of_history w =
  spr "[%s]" (String.concat "," (List.map string_of_globaltrans w))

let string_of_node = function
  | `P p -> p
  | `T t -> string_of_globaltrans t

let name_of_token t =
  spr "%s %s %s"
    (string_of_int (Unfolder.OccurrenceNet.Token.name t))
    (Unfolder.OccurrenceNet.Token.label t)
    (string_of_history (Unfolder.OccurrenceNet.Token.history t))

let label_of_token t = Unfolder.OccurrenceNet.Token.label t

let name_of_event e =
  let open Unfolder.OccurrenceNet.Event in
  spr "%s %s %s <%s>"
    (match e with
    | `E _ -> ""
    | `Rev _ -> "rev")
    (string_of_int (name e))
    (string_of_globaltrans (label e))
    (string_of_history (history e))

let label_of_event e =
  let open Unfolder.OccurrenceNet.Event in
  spr "%s %s <%s>"
    (match e with
    | `E _ -> ""
    | `Rev _ -> "rev")
    (string_of_globaltrans (label e))
    (string_of_history (history e))

let label_of_unfold_node = function
  | `P p -> label_of_token p
  | `T t -> label_of_event t

let name_of_unfold_node = function
  | `P p -> name_of_token p
  | `T t -> name_of_event t

let string_of_event e =
  let open Unfolder.OccurrenceNet.Event in
  spr "%s %s" (string_of_int (name e)) (string_of_globaltrans (label e))

let string_of_token = label_of_token

let string_of_placeset =
  string_of_set (module StringPTNetProduct.PlaceSet) Fun.id

let string_of_transset =
  string_of_set (module StringPTNetProduct.TransSet) string_of_globaltrans

let string_of_eventset =
  string_of_set (module Unfolder.OccurrenceNet.TransSet) string_of_event

let string_of_tokenset =
  string_of_set (module Unfolder.OccurrenceNet.PlaceSet) name_of_token

let print_eventset eset = print_endline (string_of_eventset eset)

let rec tword_equiv w1 w2 =
  match (w1, w2) with
  | [], [] -> true
  | [], _ :: _ | _ :: _, [] -> false
  | t1 :: u1 :: w1', u2 :: t2 :: w2'
    when t1 = t2 && u1 <> u2 && is_independent u2 t2 ->
      tword_equiv (u1 :: w1') (u2 :: w2')
  | t1 :: u1 :: w1', u2 :: t2 :: w2'
    when u1 = u2 && t1 <> t2 && is_independent t1 u1 ->
      tword_equiv (t1 :: w1') (t2 :: w2')
  | t1 :: u1 :: w1', u2 :: t2 :: w2'
    when t1 = t2 && u1 = u2 && is_independent t1 u1 ->
      tword_equiv (u1 :: w1') (u2 :: w2')
  | a1 :: w1', a2 :: w2' -> a1 = a2 && tword_equiv w1' w2'

let trace w =
  let combine ws vs =
    List.fold_left
      (fun traces w ->
        List.fold_left (fun traces v -> [ w @ v ] @ traces) [] vs @ traces)
      [] ws
  in
  let rec scramble = function
    | [] -> []
    | [ t ] -> [ [ t ] ]
    | t :: u :: r :: v' when is_independent t u && is_independent t r ->
        combine [ [ t ] ] (scramble (u :: r :: v'))
        @ combine [ [ u; t ] ] (scramble (r :: v'))
        (* exclude t to prevent swapping it with non-consecutive independents (like r) *)
    | t :: u :: v' when is_independent t u ->
        combine [ [ t ] ] (scramble (u :: v'))
        @ combine [ [ u ] ] (scramble (t :: v'))
    | t :: v' -> combine [ [ t ] ] (scramble v')
  in
  scramble w

let concat_traces w w' = trace (w @ w')

let sl_compare w w' =
  let string_of_t t =
    String.concat ""
      (List.map
         (function
           | `Idle -> ""
           | `T s -> s)
         t)
  in
  let string_of_tword w = String.concat "" (List.map string_of_t w) in
  let len_diff = List.length w - List.length w' in
  if len_diff = 0 then String.compare (string_of_tword w) (string_of_tword w')
  else len_diff

let projections w =
  let len = List.length (List.hd w) in
  assert (
    let cond = List.for_all (fun t -> List.length t = len) w in
    if not cond then
      print_endline "All global transitions must have the same length.";
    cond);
  let rec helper i projs =
    if i = len then projs else helper (i + 1) (projs @ [ projection i w ])
  in
  helper 0 []

let d_compare cmp w w' =
  List.fold_left2
    (fun res wk wk' ->
      let diff = cmp wk wk' in
      (* Compute the difference between the k-th projections, with k = 1..n;
         once it is not 0, carry it over the next steps as the final result. *)
      if res <> 0 then res else diff)
    0 (projections w) (projections w')
