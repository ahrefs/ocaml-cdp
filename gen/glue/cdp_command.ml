(* edit gen/glue/, not lib/: generate overwrites the lib/ copy *)

(** A typed protocol command: the wire method name, the encoded params, and the parser for the result. Generated command
    modules build these values; a transport sends them and types the reply:

    {[
    val call : connection -> 'result Cdp_command.t -> 'result promise
    ]} *)
type 'result t = {
  name : string;
  params : Cdp_json.t option;
  parse : Cdp_json.t -> 'result;
}
