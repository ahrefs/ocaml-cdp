let pass name = Printf.printf "PASS %s\n" name

(* 1. record decode incl. keyword field rename (type -> type_) and sealed id *)
let () =
  let json =
    Yojson.Basic.from_string
      {|{"targetId":"T1","type":"page","title":"Example","url":"https://example.com",
         "attached":true,"canAccessOpener":false}|}
  in
  let ti = Cdp.Target.target_info_of_json json in
  assert (Cdp.Target.Target_id.to_string ti.target_id = "T1");
  assert (Cdp.Target.Target_id.equal ti.target_id (Cdp.Target.Target_id.of_string "T1"));
  assert (ti.type_ = "page");
  assert (ti.attached = true);
  assert (ti.browser_context_id = None);
  pass "Target.TargetInfo decode with sealed Target_id"

(* 2. enum fallback: unknown value lands in Other, roundtrips as bare string *)
let () =
  (match Cdp.Security.security_state_of_json (`String "quantum-safe") with
  | Other { tag; payload = _ } -> assert (tag = "quantum-safe")
  | _unexpected -> assert false);
  (match Cdp.Security.security_state_of_json (`String "secure") with
  | Secure -> ()
  | _unexpected -> assert false);
  assert (Cdp.Security.security_state_to_json (Other { tag = "quantum-safe"; payload = None }) = `String "quantum-safe");
  assert (Cdp.Security.security_state_to_json Secure = `String "secure");
  pass "Security.SecurityState enum + Other fallback"

(* 3. recursive type: Runtime.StackTrace with a parent chain *)
let () =
  let json =
    Yojson.Basic.from_string
      {|{"callFrames":[{"functionName":"f","scriptId":"3","url":"https://example.com/a.js",
                        "lineNumber":10,"columnNumber":4}],
         "parent":{"callFrames":[],"description":"outer"}}|}
  in
  let st = Cdp.Runtime.stack_trace_of_json json in
  let frame = List.hd st.call_frames in
  assert (frame.function_name = "f");
  assert (Cdp.Runtime.Script_id.to_string frame.script_id = "3");
  (match st.parent with
  | Some p -> assert (p.description = Some "outer")
  | None -> assert false);
  pass "Runtime.StackTrace recursive decode"

(* 4. encode roundtrip: camelCase keys restored, None fields omitted,
      derived equal_/show_ work *)
let () =
  let ti : Cdp.Target.target_info =
    {
      target_id = Cdp.Target.Target_id.of_string "T1";
      type_ = "page";
      title = "t";
      url = "u";
      attached = true;
      can_access_opener = false;
      opener_id = None;
      opener_frame_id = None;
      browser_context_id = None;
      subtype = None;
      parent_id = None;
      parent_frame_id = None;
      embedder_data = None;
    }
  in
  let json = Cdp.Target.target_info_to_json ti in
  let keys =
    match json with
    | `Assoc kvs -> List.map fst kvs
    | _not_an_object -> []
  in
  assert (List.mem "targetId" keys);
  assert (List.mem "type" keys);
  assert (not (List.mem "browserContextId" keys));
  let ti2 = Cdp.Target.target_info_of_json json in
  assert (Cdp.Target.equal_target_info ti ti2);
  assert (String.length (Cdp.Target.show_target_info ti) > 0);
  pass "Target.TargetInfo encode roundtrip + derived equal/show"

(* 5. cross-domain ref: Network.Response carries Security.SecurityState *)
let () =
  let json =
    Yojson.Basic.from_string
      {|{"url":"https://example.com","status":200,"statusText":"OK",
         "headers":{"Content-Type":"text/html"},"mimeType":"text/html","charset":"utf-8",
         "connectionReused":false,"connectionId":12,"encodedDataLength":1234,
         "securityState":"secure"}|}
  in
  let response = Cdp.Network.response_of_json json in
  assert (response.status = 200);
  (match response.security_state with
  | Cdp.Security_types.Secure -> ()
  | _unexpected -> assert false);
  pass "Network.Response decode with cross-domain enum (int for float ok)"

(* 6. command submodule: derived make builder + params encode + wire name *)
let () =
  assert (Cdp.Network.Get_response_body.name = "Network.getResponseBody");
  let params = Cdp.Network.Get_response_body.make_params ~request_id:(Cdp.Network.Request_id.of_string "R1") in
  assert (Cdp.Network.Get_response_body.params_to_json params = `Assoc [ "requestId", `String "R1" ]);
  pass "Network.Get_response_body command submodule + make_params"

(* 7. zero-return command decodes {} to unit *)
let () =
  assert (Cdp.Network.Enable.result_of_json (`Assoc []) = ());
  assert (Cdp.Network.Enable.name = "Network.enable");
  pass "Network.Enable unit result"

(* 8. hoisted inline enum: RemoteObject.type is a variant now *)
let () =
  let json = Yojson.Basic.from_string {|{"type":"object","subtype":"null"}|} in
  let remote = Cdp.Runtime.remote_object_of_json json in
  (match remote.type_ with
  | Cdp.Runtime.Object -> ()
  | _unexpected -> assert false);
  pass "Runtime.RemoteObject hoisted inline enum"

(* 9. event submodule *)
let () =
  assert (Cdp.Page.Load_event_fired.name = "Page.loadEventFired");
  let json = Yojson.Basic.from_string {|{"timestamp":123.5}|} in
  let (ev : Cdp.Page.Load_event_fired.params) = Cdp.Page.Load_event_fired.params_of_json json in
  assert (Cdp.Network.Monotonic_time.to_float ev.timestamp = 123.5);
  pass "Page.Load_event_fired event submodule"

(* 10. typed command seam: generated modules build ready-to-send commands *)
let () =
  let params = Cdp.Network.Get_response_body.make_params ~request_id:(Cdp.Network.Request_id.of_string "R1") in
  let command = Cdp.Network.Get_response_body.command params in
  assert (command.Cdp.Command.name = "Network.getResponseBody");
  assert (command.Cdp.Command.params = Some (`Assoc [ "requestId", `String "R1" ]));
  let result = command.Cdp.Command.parse (Yojson.Basic.from_string {|{"body":"<html>","base64Encoded":false}|}) in
  assert (result.body = "<html>");
  pass "typed command seam (params, name, result parsing)"

(* 11. typed event seam, including a params-less event parsing to unit *)
let () =
  let event = Cdp.Page.Load_event_fired.event in
  assert (event.Cdp.Event.name = "Page.loadEventFired");
  let params = event.Cdp.Event.parse (Yojson.Basic.from_string {|{"timestamp":123.5}|}) in
  assert (Cdp.Network.Monotonic_time.to_float params.timestamp = 123.5);
  assert (Cdp.Page.Interstitial_shown.event.Cdp.Event.parse (`Assoc []) = ());
  pass "typed event seam (name, payload parsing, unit events)"

(* 12. unknown keys: a newer Chrome sends fields our snapshot does not know;
       records skip them instead of failing *)
let () =
  let json = Yojson.Basic.from_string {|{"x":1,"y":2,"width":3,"height":4,"fromANewerChrome":{"nested":true}}|} in
  let rect = Cdp.Dom.rect_of_json json in
  assert (rect.width = 3.0);
  let event = Cdp.Page.Load_event_fired.event in
  let params = event.Cdp.Event.parse (Yojson.Basic.from_string {|{"timestamp":1.5,"fromANewerChrome":"x"}|}) in
  assert (Cdp.Network.Monotonic_time.to_float params.timestamp = 1.5);
  pass "unknown JSON keys are skipped on types and event params"

(* 13. Chrome writes NaN and Infinity as null; a required number field
       decodes it as nan instead of failing the message, and encodes it back *)
let () =
  assert (Float.is_nan (Cdp_json.number_of_json `Null));
  assert (Cdp_json.number_to_json Float.nan = `Null);
  assert (Cdp_json.number_to_json Float.infinity = `Null);
  assert (Cdp_json.number_to_json 1.5 = `Float 1.5);
  let event = Cdp.Page.Load_event_fired.event in
  let params = event.Cdp.Event.parse (Yojson.Basic.from_string {|{"timestamp":null}|}) in
  assert (Float.is_nan (Cdp.Network.Monotonic_time.to_float params.timestamp));
  pass "null in a number field decodes as nan"

let () = print_endline "all tests passed"
