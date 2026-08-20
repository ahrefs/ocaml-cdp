(* A minimal renderer: given a URL,
   navigate a headless Chrome and report the main document's HTTP status,
   headers, and the rendered HTML. Usage:

     dune exec examples/render.exe -- https://example.com *)

type document_response = {
  status : int;
  mime_type : string;
  headers : Cdp_json.t;
  loader_id : Cdp.Network.Loader_id.t;
}

let () =
  let url =
    match Array.to_list Sys.argv with
    | _program :: requested :: _rest -> requested
    | _no_argument -> "https://example.com"
  in
  Lwt_main.run
    begin
      let%lwt chrome = Cdp_lwt.Chrome.launch () in
      let%lwt transport = Cdp_lwt.Curl_transport.connect ~url:chrome.ws_url () in
      let connection = Cdp_lwt.Connection.create transport in
      let call ?session command = Cdp_lwt.Connection.call connection ?session ~timeout:15.0 command in

      let%lwt created =
        call (Cdp.Target.Create_target.command (Cdp.Target.Create_target.make_params ~url:"about:blank" ()))
      in
      let%lwt attached =
        call
          (Cdp.Target.Attach_to_target.command
             (Cdp.Target.Attach_to_target.make_params ~target_id:created.target_id ~flatten:true ()))
      in
      let session = attached.session_id in
      let%lwt () = call ~session (Cdp.Page.Enable.command (Cdp.Page.Enable.make_params ())) in
      let%lwt () = call ~session (Cdp.Network.Enable.command (Cdp.Network.Enable.make_params ())) in
      (* collect every main-document response; real pages answer with many
         responseReceived events (subresources), so this must be persistent *)
      let document_responses = ref [] in
      let unsubscribe =
        Cdp_lwt.Connection.on_event connection ~session Cdp.Network.Response_received.event (fun received ->
          match received.type_ with
          | Cdp.Network.Document ->
            document_responses :=
              {
                status = received.response.status;
                mime_type = received.response.mime_type;
                headers = received.response.headers;
                loader_id = received.loader_id;
              }
              :: !document_responses
          | _subresource -> ())
      in
      let loaded = Cdp_lwt.Connection.next_event connection ~session Cdp.Page.Load_event_fired.event in
      let%lwt navigation = call ~session (Cdp.Page.Navigate.command (Cdp.Page.Navigate.make_params ~url ())) in
      let%lwt _fired = loaded in
      unsubscribe ();
      (* the main document is the one belonging to the navigation's loader *)
      let main_document =
        match navigation.loader_id with
        | Some navigation_loader ->
          List.find_opt
            (fun candidate -> Cdp.Network.Loader_id.equal candidate.loader_id navigation_loader)
            !document_responses
        | None ->
        match List.rev !document_responses with
        | first_seen :: _rest -> Some first_seen
        | [] -> None
      in
      let%lwt evaluated =
        call ~session
          (Cdp.Runtime.Evaluate.command
             (Cdp.Runtime.Evaluate.make_params ~expression:"document.documentElement.outerHTML" ()))
      in
      let html =
        match evaluated.result.value with
        | Some (`String rendered) -> rendered
        | _not_a_string -> ""
      in
      (match main_document with
      | None -> print_endline "no document response captured"
      | Some document ->
        Printf.printf "url:          %s\n" url;
        Printf.printf "status:       %d\n" document.status;
        Printf.printf "mime type:    %s\n" document.mime_type;
        Printf.printf "headers:      %d\n"
          (match document.headers with
          | `Assoc fields -> List.length fields
          | _not_an_object -> 0);
        Printf.printf "html bytes:   %d\n" (String.length html);
        Printf.printf "html preview: %s...\n" (String.sub html 0 (min 80 (String.length html))));

      let%lwt () = Cdp_lwt.Connection.close connection in
      chrome.kill ()
    end
