# Unreleased

- `cdp-gen`: `--version`.

- `cdp-gen`: `generate` also writes the four glue modules the generated files
  need (`cdp_json.ml`, `cdp_command.ml`, `cdp_event.ml`, `cdp_envelope.ml`),
  so the output directory compiles on its own. Nothing to copy from `lib/`.

- `cdp-gen`: `--help` on every command, paths checked up front, and any
  number of protocol files instead of exactly two.

- `cdp`: `Cdp.Envelope.build_request ~id ?session command` builds the wire
  request straight from a typed command, for transports other than `cdp-lwt`.

- `cdp-gen`: when the selected domains point at domains outside the
  selection, one message now lists all of them, for example
  `Fetch also needs Network,IO,Page,Runtime,Debugger,Emulation,DOM,Security`,
  instead of one missing domain per run.

- `cdp`, `cdp-gen`: protocol descriptions are now doc comments on every
  domain, type, record field, command and event, so Merlin hover and odoc
  show the Chrome text.

- `cdp`: a protocol `number` field is now `Cdp_json.number`, an alias of
  `float`. Chrome writes NaN and Infinity as `null`; such a value decodes as
  `nan` instead of failing the whole message, and encodes back as `null`.
  Sealed number ids such as `Network.Monotonic_time` do the same.

- `cdp`: two enum types are renamed because they shared a name with
  `Page.navigation_type`: `Page.Frame_started_navigating.navigation_type` is
  now `frame_started_navigating_navigation_type`, and
  `Page.Navigated_within_document.navigation_type` is now
  `navigated_within_document_navigation_type`. Field names are unchanged.

- `cdp`, `cdp-gen`: deprecated and experimental flags now also reach record
  fields (`[@ocaml.deprecated]`, `[@alert experimental]` on the field) and
  whole domains (`[@@@...]` at the top of the domain files and on the `Cdp`
  index aliases).
  Reading a deprecated field such as `Network.Response.headers_text` now warns.
  Two call sites of the deprecated `Network.setCookie` `success` field were removed from the examples and the browser test.

- `cdp`, `cdp-gen`: a command with a protocol `redirect` names the domain that
  now handles it. Deprecated ones say `deprecated in CDP, redirected to the
  Emulation domain`; the five that are not deprecated (for example
  `DOM.hideHighlight`) get `[@@alert redirected "..."]`. Silence it with
  `-alert -redirected` if you rely on them.

- `cdp-gen`: `generate` removes generated `cdp_*.ml` files that the current
  run does not produce, so a domain dropped from the selection no longer
  leaves stale files that break the next build. Files without the generator
  header are left alone.

# 0.1.0 (2026-08-29)

- Initial release
- `cdp`: typed commands, events, and JSON codecs for 10 Chrome DevTools
  Protocol domains (Browser, DOM, Debugger, Emulation, IO, Network, Page,
  Runtime, Security, Target), generated from protocol r1687809
- `cdp-gen`: the generator behind `cdp` — fetches a devtools-protocol
  snapshot and emits typed OCaml modules for the selected domains
- `cdp-lwt`: Lwt client — launches or attaches to a Chrome and drives it
  over libcurl WebSockets. Not yet published to opam: it needs an ocurl
  release with the libcurl WebSocket API (`pin-depends` covers source builds)
