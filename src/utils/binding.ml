module type Applicative_syntax = sig
  type 'a t

  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  val return : 'a -> 'a t
end

module type Monad_syntax = sig
  include Applicative_syntax

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( and* ) : 'a t -> 'b t -> ('a * 'b) t
end

module OptionMonad : Monad_syntax = struct
  type 'a t = 'a Option.t

  let ( let+ ) x f = Option.map f x
    
  let prod a b =
    match a, b with
    | None, _ | _, None -> None
    | Some a, Some b -> Some (a, b)

  let ( and+ ) = prod
  let return = Option.some;;
  
  let ( let* ) = Option.bind
  let ( and* ) = prod
end

(** Support for element-wise operations on multiple sequences *)
module ZipSeq : Applicative_syntax = struct
  type 'a t = 'a Seq.t

  open Seq

  let rec return x () = Cons (x, return x)

  let rec prod a b () =
    match (a (), b ()) with
    | Nil, _ | _, Nil -> Nil
    | Cons (x, a), Cons (y, b) -> Cons ((x, y), prod a b)

  let ( let+ ) f s = map s f
  let ( and+ ) a b = prod a b
  
  (* [and+] occurs in the body expression of [let+], so the
   sequences will be zipped before being mapped *)
end
