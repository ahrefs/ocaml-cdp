(* Send-loop tests with an injected ws_send: short sends, stalls, mid-frame failure, and transport death. *)

let pass name = Printf.printf "PASS %s\n" name

exception Socket_full

let always_alive = ref true
let no_death () = assert false
let no_abort (_failure : exn) = assert false

let () =
  Lwt_main.run
    begin
      (* 1. short sends: at most 7 bytes accepted per call — every byte must
         arrive exactly once, in order *)
    let accepted = Buffer.create 64 in
    let short_sender piece =
      let taken = min 7 (String.length piece) in
      Buffer.add_string accepted (String.sub piece 0 taken);
      taken
    in
    let payload = "the quick brown fox jumps over the lazy dog" in
    let%lwt () =
      Cdp_lwt.Curl_transport.send_all ~alive:always_alive ~death_error:no_death ~abort:no_abort ~ws_send:short_sender
        payload
    in
    assert (Buffer.contents accepted = payload);
    pass "short sends deliver every byte exactly once, in order";

    (* 2. a stall mid-payload, then progress: budget must reset and the
         payload must still arrive complete *)
    let accepted = Buffer.create 64 in
    let stalls = ref 3 in
    let stalling_sender piece =
      match !stalls with
      | 0 ->
        let taken = min 5 (String.length piece) in
        Buffer.add_string accepted (String.sub piece 0 taken);
        stalls := 3;
        taken
      | _still_stalling ->
        decr stalls;
        raise Socket_full
    in
    let%lwt () =
      Cdp_lwt.Curl_transport.send_all ~stall_budget:4 ~alive:always_alive ~death_error:no_death ~abort:no_abort
        ~ws_send:stalling_sender "twelve bytes"
    in
    assert (Buffer.contents accepted = "twelve bytes");
    pass "stalls are retried and the budget resets on progress";

    (* 3. nothing ever accepted: fails after the budget, and abort must NOT
         run — no frame was started, the transport is still usable *)
    let aborted = ref false in
    let%lwt () =
      try%lwt
        let%lwt () =
          Cdp_lwt.Curl_transport.send_all ~stall_budget:2 ~alive:always_alive ~death_error:no_death
            ~abort:(fun _failure -> aborted := true)
            ~ws_send:(fun _piece -> raise Socket_full)
            "never sent"
        in
        assert false
      with Socket_full -> Lwt.return_unit
    in
    assert (not !aborted);
    pass "exhaustion before the first byte fails without killing the transport";

    (* 4. failure after partial progress: the frame is half on the wire, so
         the whole transport must be aborted with that failure *)
    let aborted_with = ref None in
    let first_call = ref true in
    let%lwt () =
      try%lwt
        let%lwt () =
          Cdp_lwt.Curl_transport.send_all ~stall_budget:2 ~alive:always_alive ~death_error:no_death
            ~abort:(fun failure -> aborted_with := Some failure)
            ~ws_send:(fun piece ->
              match !first_call with
              | true ->
                first_call := false;
                min 4 (String.length piece)
              | false -> raise Socket_full)
            "half sent frame"
        in
        assert false
      with Socket_full -> Lwt.return_unit
    in
    (match !aborted_with with
    | Some Socket_full -> ()
    | _no_abort_or_wrong_failure -> assert false);
    pass "exhaustion mid-frame aborts the transport with the failure";

    (* 5. the transport dies during a stall: the send must fail with the
         transport's death error, not keep retrying *)
    let alive = ref true in
    let%lwt () =
      try%lwt
        let%lwt () =
          Cdp_lwt.Curl_transport.send_all ~stall_budget:10 ~alive
            ~death_error:(fun () -> Cdp_lwt.Transport.Closed)
            ~abort:no_abort
            ~ws_send:(fun _piece ->
              alive := false;
              raise Socket_full)
            "dies mid-send"
        in
        assert false
      with Cdp_lwt.Transport.Closed -> Lwt.return_unit
    in
    pass "transport death during a stall fails the send typed";
    Lwt.return_unit
    end

let () = print_endline "all send_all tests passed"
