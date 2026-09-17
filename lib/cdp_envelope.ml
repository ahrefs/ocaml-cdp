(* The CDP wire envelope. Outgoing requests are JSON objects
   {id, method, params?, sessionId?}; incoming messages are either responses
   {id, result | error, sessionId?} or events {method, params?, sessionId?}.
   Sessions are raw strings here so this file depends on no generated code;
   transports may expose them as Cdp.Target.Session_id.t at their boundary. *)

type error = {
  code : int;
  message : string;
  data : Cdp_json.t option;
}

type incoming =
  | Response of {
      id : int;
      outcome : (Cdp_json.t, error) result;
      session : string option;
    }
  | Event of {
      name : string;
      params : Cdp_json.t;
      session : string option;
    }

let request ~id ?session ~name ~params () : Cdp_json.t =
  `Assoc
    (List.filter_map
       (fun field -> field)
       [
         Some ("id", `Int id);
         Some ("method", `String name);
         (match params with
         | None -> None
         | Some payload -> Some ("params", payload));
         (match session with
         | None -> None
         | Some session_id -> Some ("sessionId", `String session_id));
       ])

(* for a transport to not take the command apart itself *)
let build_request ~id ?session (command : _ Cdp_command.t) : Cdp_json.t =
  request ~id ?session ~name:command.name ~params:command.params ()

let parse (json : Cdp_json.t) : (incoming, string) result =
  let member = Yojson.Basic.Util.member in
  let malformed_error () = Error ("cdp: malformed error object: " ^ Yojson.Basic.to_string json) in
  let session_of json =
    match member "sessionId" json with
    | `String session_id -> Some session_id
    | _absent -> None
  in
  (* the error code is an integer, but tolerate servers sending it as a float *)
  let error_fields error_json =
    match member "code" error_json, member "message" error_json with
    | `Int code, `String message -> Some (code, message)
    | `Float code, `String message -> Some (int_of_float code, message)
    | _malformed -> None
  in
  match member "id" json, member "method" json with
  | exception Yojson.Basic.Util.Type_error (_reason, _value) ->
    Error ("cdp: message is not a JSON object: " ^ Yojson.Basic.to_string json)
  | `Int id, `Null ->
    let session = session_of json in
    (match member "error" json with
    | `Null -> Ok (Response { id; outcome = Ok (member "result" json); session })
    | error_json ->
    match error_fields error_json with
    | exception Yojson.Basic.Util.Type_error (_reason, _value) -> malformed_error ()
    | None -> malformed_error ()
    | Some (code, message) ->
      let data =
        match member "data" error_json with
        | `Null -> None
        | payload -> Some payload
      in
      Ok (Response { id; outcome = Error { code; message; data }; session }))
  | `Null, `String name -> Ok (Event { name; params = member "params" json; session = session_of json })
  | _not_a_valid_message -> Error ("cdp: message is not a valid response or event: " ^ Yojson.Basic.to_string json)
