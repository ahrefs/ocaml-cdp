(* edit gen/glue/, not lib/: generate overwrites the lib/ copy *)

(** A typed protocol event: the wire event name and the parser for its payload. Generated event modules build these
    values; a transport uses them to subscribe with typed callbacks:

    {[
    val next_event : connection -> 'params Cdp_event.t -> 'params promise
    ]} *)
type 'params t = {
  name : string;
  parse : Cdp_json.t -> 'params;
}
