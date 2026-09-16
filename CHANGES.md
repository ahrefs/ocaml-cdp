# Unreleased

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
