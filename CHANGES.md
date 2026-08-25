# Unreleased

- Initial release
- `cdp`: typed commands, events, and JSON codecs for 10 Chrome DevTools
  Protocol domains (Browser, DOM, Debugger, Emulation, IO, Network, Page,
  Runtime, Security, Target), generated from protocol r1680125
- `cdp-gen`: the generator behind `cdp` — fetches a devtools-protocol
  snapshot and emits typed OCaml modules for the selected domains
- `cdp-lwt`: Lwt client — launches or attaches to a Chrome and drives it
  over libcurl WebSockets
