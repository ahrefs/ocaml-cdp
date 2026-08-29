(* Frame-reassembly tests: chunked frames, fragmented messages, interleaved
   control frames — sequences a fast localhost Chrome never produces. *)

open Cdp_lwt.Curl_transport

let pass name = Printf.printf "PASS %s\n" name

let () =
  let message_buffer = Buffer.create 64 in
  let feed = accumulate ~message_buffer in

  (* a whole message in one chunk *)
  assert (feed ~chunk:"hello" ~is_payload:true ~is_final:true = Complete "hello");
  pass "an unfragmented single-chunk message completes at once";

  (* one large frame arriving in several chunks (bytesleft > 0 until the last) *)
  assert (feed ~chunk:"one " ~is_payload:true ~is_final:false = Accumulating);
  assert (feed ~chunk:"large " ~is_payload:true ~is_final:false = Accumulating);
  assert (feed ~chunk:"frame" ~is_payload:true ~is_final:true = Complete "one large frame");
  pass "a frame split into chunks reassembles in order";

  (* a fragmented message (continuation frames), with a control frame's
     chunk interleaved mid-message: control payloads must not leak in *)
  assert (feed ~chunk:"first|" ~is_payload:true ~is_final:false = Accumulating);
  assert (feed ~chunk:"PING" ~is_payload:false ~is_final:false = Ignored);
  assert (feed ~chunk:"second|" ~is_payload:true ~is_final:false = Accumulating);
  assert (feed ~chunk:"third" ~is_payload:true ~is_final:true = Complete "first|second|third");
  pass "fragments reassemble across an interleaved control frame";

  (* the buffer resets between messages: nothing bleeds into the next one *)
  assert (feed ~chunk:"fresh" ~is_payload:true ~is_final:true = Complete "fresh");
  pass "the buffer starts clean after each completed message";

  (* an empty final chunk still closes the message *)
  assert (feed ~chunk:"tail" ~is_payload:true ~is_final:false = Accumulating);
  assert (feed ~chunk:"" ~is_payload:true ~is_final:true = Complete "tail");
  pass "an empty final chunk completes the pending message";

  print_endline "all reassembly tests passed"
