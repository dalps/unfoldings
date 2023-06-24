type local_t = Idle | T of string

(** e.g. <t1,ϵ> is encoded as [T "t1";Idle]*)
type t = local_t list

val string_of_t : t -> string

val is_idle : local_t -> bool

(** e.g. participates 1 <t1,ϵ> = true *)
val participates : int -> t -> bool

(** Project a global transition onto the i-th product component.
    e.g. projection 2 [<t1,ϵ>] = [] 
 *)
val projection : int -> t list -> t list

(** Project a global transition word onto every product component.
    e.g. projections [<t1,ϵ>;<ϵ,u1>] = [[<t1,ϵ>]; [<ϵ,u1>]] 
 *)
val projections : t list -> t list list

(* e.g. is_independent <t1,ϵ> <ϵ,u1> = true *)
val is_independent : t -> t -> bool

(** Test if w2 can be obtained from w1 by swapping
   consecutive indepentent transitions.
   e.g. tword_equiv [<t1,ϵ>;<ϵ,u1>] [<ϵ,u1>;<t1,ϵ>] = true 
 *)
val tword_equiv : t list -> t list -> bool

(** Given w, enumerate all words w' such that w' tword equiv w.
    e.g. trace [<t1,ϵ>;<ϵ,u1>] = [[<t1,ϵ>;<ϵ,u1>]; [<ϵ,u1>;<t1,ϵ>]]
 *)
val trace : t list -> t list list

(** concat_traces w1 w2 = trace (w1 @ w2) *)
val concat_traces : t list -> t list -> t list list

val sl_compare : t list -> t list -> int

val d_compare : (t list -> t list -> int) -> t list -> t list -> int

val compare : t -> t -> int
