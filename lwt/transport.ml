(* A transport is anything that moves whole text messages in both directions.
   The connection logic is written against this record, so tests drive it
   with an in-memory fake and production uses the libcurl WebSocket. *)

type t = {
  send : string -> unit Lwt.t;
  receive : unit -> string option Lwt.t;  (** next complete incoming message; [None] means the transport is closed *)
  close : unit -> unit Lwt.t;
}
