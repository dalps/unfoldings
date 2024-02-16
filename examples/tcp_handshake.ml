module HostEvent = struct
  type event = CONNECT | LISTEN | SEND | CLOSE | SYN | FIN | ACK | RST
  type action = SYN | FIN | ACK | RST | NO_ACTION
  type t = event list * action list

  let compare = compare
end

module TCPState = struct
  type t = CLOSED | LISTEN | SYN_RCVD | SYN_SENT | ESTABLISHED

  let compare = compare
end

(**
   A normal TCP connection
*)
let listen : HostEvent.t = ([ LISTEN ], [ NO_ACTION ])
(* 1. server is listening *)

let h1 : HostEvent.t = ([ CONNECT ], [ SYN ])
(* 2. client sends <SYN=1; ACK=0> *)

let h2 : HostEvent.t = ([ SYN ], [ SYN; ACK ])
(* 3. server sends <SYN=1; ACK=1> *)

let h3 : HostEvent.t = ([ SYN; ACK ], [ ACK ])
(* 4. clients sends <ACK=1> with application data *)

let ack_rcvd : HostEvent.t = ([ ACK ], [ NO_ACTION ])
(* 5. connection established *)

(**
   Edge cases    
*)
let close : HostEvent.t = ([ CLOSE ], [ NO_ACTION ])
(* Some host changes its mind *)

let send_syn : HostEvent.t = ([ SEND ], [ SYN ])
(* Used in a simultaneous handshake *)

let refuse : HostEvent.t = ([ RST ], [ NO_ACTION ])
(* There's no TCP entity listening on the requested port *)

module TCPEntity = Petrilib.Petrinet.Make (TCPState) (HostEvent)
open TCPEntity

(**
   TCP connection automaton
*)
let tcp =
  of_lists
    [ CLOSED; LISTEN; SYN_RCVD; SYN_SENT; ESTABLISHED ]
    [ close; listen; h1; h2; h3; ack_rcvd; refuse; send_syn ]
    [
      ([ CLOSED ] --> [ LISTEN ]) listen;
      ([ CLOSED ] --> [ SYN_SENT ]) h1;
      ([ LISTEN ] --> [ SYN_RCVD ]) h2;
      ([ SYN_SENT ] --> [ ESTABLISHED ]) h3;
      ([ SYN_RCVD ] --> [ ESTABLISHED ]) ack_rcvd;
      ([ LISTEN ] --> [ CLOSED ]) close;
      ([ SYN_SENT ] --> [ CLOSED ]) close;
      (* ([ LISTEN; SYN_SENT ] --> [CLOSED]) close; *)
      ([ SYN_RCVD ] --> [ LISTEN ]) refuse;
      ([ LISTEN ] --> [ SYN_SENT ]) send_syn;
      ([ SYN_SENT ] --> [ SYN_RCVD ]) h2;
    ]
    [ CLOSED ]
