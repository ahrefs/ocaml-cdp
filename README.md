# ocaml-cdp

OCaml client for the [Chrome DevTools Protocol](https://chromedevtools.github.io/devtools-protocol/)
(CDP): typed protocol modules generated from the official protocol JSON, plus
an Lwt connection over libcurl WebSockets.

**Status: 0.1.0.** `cdp` and `cdp-gen` are on opam; `cdp-lwt` builds from
source for now (see below). The full stack works end to end against a real
Chrome (see the demo below); the API may still change.

## Packages

| Package | What it is |
|---|---|
| `cdp` | Typed protocol domains: records, enums, commands, events, with JSON codecs |
| `cdp-lwt` | The connection: WebSocket transport, typed `call` and `next_event` |
| `cdp-gen` | The generator CLI: protocol JSON in, OCaml out |

`cdp` ships 10 domains: Browser, DOM, Debugger, Emulation, IO, Network, Page,
Runtime, Security, and Target. The generator covers all 58 — see
[How generation works](#how-generation-works) to build your own selection.

## Install

The protocol library and the generator are on opam:

```sh
opam install cdp cdp-gen
```

`cdp-lwt` is not on opam yet: it uses libcurl's WebSocket API, which ocurl
has not released yet (latest release: 0.10.0). Until then, build the full
stack from a clone:

```sh
git clone https://github.com/ahrefs/ocaml-cdp.git
cd ocaml-cdp
opam switch create . 5.4.1 --no-install
opam pin add -yn . --with-version dev
opam install . --deps-only --with-test
make build test
```

The `opam pin` line gives all three packages the same `dev` version — without
it, opam picks the released `0.1.0` for `cdp` but `dev` for `cdp-lwt`, and
`cdp-lwt`'s exact-version dependency on `cdp` cannot be solved. The pin also
brings in ocurl master via `cdp-lwt.opam`'s `pin-depends`, and `opam install`
picks everything up by itself.

Your system libcurl must be 7.86 or newer (check with
`curl-config --version`) and built with WebSocket support. Chrome-launching code and examples need a Chrome:
by default the executable is `google-chrome` from `PATH`; set the
`CDP_CHROME` environment variable (or pass `~executable` to `Chrome.launch`)
to use another binary, e.g. `chromium` or
`CDP_CHROME="/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"`
on macOS.

## Demo

`make demo` launches a headless Chrome and runs [examples/navigate.ml](examples/navigate.ml):

```ocaml
let%lwt chrome = Cdp_lwt.Chrome.launch () in
let%lwt transport = Cdp_lwt.Curl_transport.connect ~url:chrome.ws_url () in
let connection = Cdp_lwt.Connection.create transport in
let call ?session command = Cdp_lwt.Connection.call connection ?session ~timeout:10.0 command in

let%lwt created = call (Cdp.Target.Create_target.command (Cdp.Target.Create_target.make_params ~url:"about:blank" ())) in
let%lwt attached =
  call (Cdp.Target.Attach_to_target.command
          (Cdp.Target.Attach_to_target.make_params ~target_id:created.target_id ~flatten:true ()))
in
let session = attached.session_id in

let%lwt () = call ~session (Cdp.Page.Enable.command (Cdp.Page.Enable.make_params ())) in
let loaded = Cdp_lwt.Connection.next_event connection ~session Cdp.Page.Load_event_fired.event in
let%lwt _navigation = call ~session (Cdp.Page.Navigate.command (Cdp.Page.Navigate.make_params ~url:"data:text/html,<title>Hello from OCaml CDP</title>" ())) in
let%lwt _fired = loaded in

let%lwt evaluated =
  call ~session (Cdp.Runtime.Evaluate.command (Cdp.Runtime.Evaluate.make_params ~expression:"document.title" ()))
in
```

Every step is a typed command; failures are typed too (`Protocol_error`,
`Call_timeout`, `Session_detached`, `Connection_closed`). `call` waits up to
180 seconds by default — pass `~timeout` to change it (the demo shortens it
to 10), or `Float.infinity` to wait forever.

**One rule to know:** `next_event` catches events arriving *after* it is
called — subscribe first, then trigger (as the demo does around `navigate`).
`next_event` waits for one occurrence; for a persistent subscription use
`Connection.on_event`, which fires on every occurrence until unsubscribed.

## Examples

All examples live in [examples/](examples/) and run against a real Chrome:

| Example | Command | What it does |
|---|---|---|
| [navigate](examples/navigate.ml) | `make demo` | launch Chrome, open a page, read its title back |
| [render](examples/render.ml) | `make render URL=https://example.com` | report the main document's HTTP status, headers, and rendered HTML |
| [screenshot](examples/screenshot.ml) | `make screenshot URL=https://example.com NAME=shot` | save a full-size screenshot and a small thumbnail into `screenshots/` |
| [attach](examples/attach.ml) | `make attach WS=<websocket url>` | screenshot through a Chrome that is already running, closing only its own tab |

`navigate`, `render`, and `screenshot` launch their own headless Chrome.
`attach` connects to an existing one: start Chrome with
`--remote-debugging-port=<port>`, read `webSocketDebuggerUrl` from
`http://127.0.0.1:<port>/json/version`, and pass it as `WS=`.

`Chrome.launch` keeps Chrome's sandbox on — it is the isolation layer between
web pages and your machine. In environments where the sandbox cannot start
(typically running as root in a container without user namespaces), pass
`~no_sandbox:true` and treat every page you open as untrusted.

## The types

- Ids and timestamps are sealed: a `Request_id.t` cannot be confused with a
  `Frame_id.t`.
- Enums are variants with an `Other` fallback, so new Chrome enum values
  never crash a decode.
- Optional fields are `option`; `None` fields are omitted on the wire.
- Deprecated and experimental protocol items carry compiler alerts.
- Broken characters from pages are repaired, not fatal: JavaScript strings
  may hold half of a two-unit character (`"😀".substring(0, 1)`), and Chrome
  sends the lone half as-is. OCaml strings are UTF-8 and cannot represent
  it, and dropping the message would hang the pending call — so each
  unpaired half becomes U+FFFD (`�`) before parsing.

## How generation works

```
protocol/*.json  --(cdp-gen)-->  lib/cdp_*.ml  --(dune + ppx)-->  the cdp library
```

The generated code is committed; users never run the generator. The vendored
protocol snapshot is pinned in `protocol/REVISION`.

```sh
make generate                 # regenerate lib/ after changing gen/gen.ml
make update-protocol          # fetch the latest protocol and regenerate
make update-protocol REV=1680125  # fetch a specific revision
make check                    # CI guard: lib/ matches the generator
```

To generate types for your own protocol snapshot (any revision, or your own
JSON), use the CLI directly:

```sh
cdp-gen fetch mydir 1650000
cdp-gen generate mydir/browser_protocol.json mydir/js_protocol.json out Network,Page
cdp-gen generate --help       # every argument explained
```

If a selected domain refers to domains outside the list, one message names
all of them, for example `Fetch also needs DOM,Debugger,Emulation,IO,Network,Page,Runtime,Security`.

To compile the output as a library: copy the four hand-written glue files
`cdp_json.ml`, `cdp_command.ml`, `cdp_event.ml`, and `cdp_envelope.ml` from
[lib/](lib/) next to the generated files, and use this dune stanza (the one
`make check-full` uses, under whatever library name you like):

```
(library
 (name my_cdp)
 (wrapped false)
 (libraries jsonkit yojson)
 (preprocess
  (pps jsonkit.ppx ppx_deriving.show ppx_deriving.eq ppx_deriving.make))
 (flags
  (:standard -w -a -alert -all)))
```

## Development

`make help` lists all targets. Tests:

- cram tests (`test/cram/*.t`): small protocol JSON in, generated OCaml out —
  review diffs with `dune runtest`, accept with `dune promote`;
- generator unit tests (`test/gen/`) and schema-driven roundtrip tests over
  every generated type (`test/roundtrip/`, regenerated by `make generate`);
- envelope and connection tests over a mock transport (`test/`, `test/lwt/`);
- `make test-browser`: opt-in smoke tests against a local headless Chrome;
- `make check-full`: generates and compiles ALL 58 protocol domains.

## License

MIT, with one exception: the vendored protocol definitions
`protocol/browser_protocol.json` and `protocol/js_protocol.json` come from the
[Chrome DevTools Protocol](https://github.com/ChromeDevTools/devtools-protocol)
and are BSD-3-Clause, Copyright 2014 The Chromium Authors — see
[protocol/LICENSE](protocol/LICENSE). `make update-protocol` refreshes that
license file together with the JSONs.
