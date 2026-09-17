(* Unit tests for the CDP wire envelope: request encoding and incoming
   message classification. *)

let pass name = Printf.printf "PASS %s\n" name

let () =
  let json = Cdp.Envelope.request ~id:7 ~name:"Page.enable" ~params:None () in
  assert (json = `Assoc [ "id", `Int 7; "method", `String "Page.enable" ]);
  let json =
    Cdp.Envelope.request ~id:8 ~session:"SESSION1" ~name:"Page.navigate"
      ~params:(Some (`Assoc [ "url", `String "https://example.com" ]))
      ()
  in
  assert (
    json
    = `Assoc
        [
          "id", `Int 8;
          "method", `String "Page.navigate";
          "params", `Assoc [ "url", `String "https://example.com" ];
          "sessionId", `String "SESSION1";
        ]);
  pass "request encoding, with and without params and session"

let () =
  let params = Cdp.Network.Get_response_body.make_params ~request_id:(Cdp.Network.Request_id.of_string "R1") in
  let command = Cdp.Network.Get_response_body.command params in
  let json = Cdp.Envelope.build_request ~id:9 ~session:"SESSION1" command in
  assert (
    Yojson.Basic.to_string json
    = {|{"id":9,"method":"Network.getResponseBody","params":{"requestId":"R1"},"sessionId":"SESSION1"}|});
  let bare = Cdp.Envelope.build_request ~id:10 Cdp.Runtime.Enable.command in
  assert (Yojson.Basic.to_string bare = {|{"id":10,"method":"Runtime.enable"}|});
  pass "build_request takes a typed command apart"

let () =
  (match Cdp.Envelope.parse (Yojson.Basic.from_string {|{"id":7,"result":{"ok":true}}|}) with
  | Ok (Response { id = 7; outcome = Ok (`Assoc [ ("ok", `Bool true) ]); session = None }) -> ()
  | _unexpected -> assert false);
  (match Cdp.Envelope.parse (Yojson.Basic.from_string {|{"id":0,"result":{},"sessionId":"S9"}|}) with
  | Ok (Response { id = 0; outcome = Ok (`Assoc []); session = Some "S9" }) -> ()
  | _unexpected -> assert false);
  (match Cdp.Envelope.parse (Yojson.Basic.from_string {|{"id":3}|}) with
  | Ok (Response { id = 3; outcome = Ok `Null; session = None }) -> ()
  | _unexpected -> assert false);
  pass "responses: result, id zero, session carried, missing result is Null"

let () =
  (match Cdp.Envelope.parse (Yojson.Basic.from_string {|{"id":9,"error":{"code":-32000,"message":"boom"}}|}) with
  | Ok (Response { id = 9; outcome = Error { code = -32000; message = "boom"; data = None }; session = None }) -> ()
  | _unexpected -> assert false);
  (match
     Cdp.Envelope.parse
       (Yojson.Basic.from_string {|{"id":10,"error":{"code":-32000.0,"message":"boom","data":{"why":"y"}}}|})
   with
  | Ok (Response { id = 10; outcome = Error { code = -32000; data = Some (`Assoc [ ("why", `String "y") ]); _ }; _ }) ->
    ()
  | _unexpected -> assert false);
  pass "response errors: plain, and float code with a data payload"

let () =
  (match
     Cdp.Envelope.parse
       (Yojson.Basic.from_string {|{"method":"Page.loadEventFired","params":{"timestamp":1.5},"sessionId":"S1"}|})
   with
  | Ok (Event { name = "Page.loadEventFired"; params = `Assoc [ ("timestamp", `Float 1.5) ]; session = Some "S1" }) ->
    ()
  | _unexpected -> assert false);
  (match Cdp.Envelope.parse (Yojson.Basic.from_string {|{"method":"Target.ping"}|}) with
  | Ok (Event { name = "Target.ping"; params = `Null; session = None }) -> ()
  | _unexpected -> assert false);
  pass "events, with and without params and session"

let () =
  let is_error raw =
    match Cdp.Envelope.parse (Yojson.Basic.from_string raw) with
    | Error _reason -> true
    | Ok _classified -> false
  in
  assert (is_error {|"just a string"|});
  assert (is_error {|{"neither":"nor"}|});
  assert (is_error {|{"id":1,"error":"not an object"}|});
  assert (is_error {|{"id":1,"method":"Page.enable"}|});
  pass "garbage and malformed messages are rejected, not raised"

let () = print_endline "all envelope tests passed"
