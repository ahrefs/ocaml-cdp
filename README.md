# ocaml-cdp

OCaml types for the [Chrome DevTools Protocol](https://chromedevtools.github.io/devtools-protocol/)
(CDP), generated from the official protocol JSON.

**Status: experimental, not released.** Types compile and roundtrip; the
transport layer (WebSocket client) does not exist yet.

## Packages

| Package | What it is |
|---|---|
| `cdp` | Typed protocol domains: records, enums, commands, events, with JSON codecs |
| `cdp-gen` | The generator CLI: protocol JSON in, OCaml out |

## Example

```ocaml
let p =
  Cdp.Network.Get_response_body.make_params
    ~request_id:(Cdp.Network.Request_id.of_string "R1")
in
send Cdp.Network.Get_response_body.name
  (Cdp.Network.Get_response_body.params_to_json p)
```

- Ids and timestamps are sealed: a `Request_id.t` cannot be confused with a
  `Frame_id.t`.
- Enums are variants with an `Other` fallback, so new Chrome enum values
  never crash a decode.
- Optional fields are `option`; `None` fields are omitted on the wire.
- Deprecated and experimental protocol items carry compiler alerts.

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
```

## Development

`make help` lists all targets. Tests are golden cram tests
(`test/golden/*.t`: small protocol JSON in, generated OCaml out — review
diffs with `dune runtest`, accept with `dune promote`) plus runtime
decode/encode tests (`test/test_decode.ml`).
