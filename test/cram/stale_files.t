A domain dropped from the selection must not leave its files from an earlier
run in the output directory. A user's own cdp_*.ml without the generator
header is left alone.

  $ cat > browser.json << 'EOF'
  > {"domains":[
  >   {"domain":"Alpha","types":[{"id":"AlphaId","type":"string"}]},
  >   {"domain":"Beta","types":[{"id":"BetaId","type":"string"}]}
  > ]}
  > EOF
  $ echo '{"domains":[]}' > js.json
  $ mkdir out && echo '(* my own helper *)' > out/cdp_my_helper.ml
  $ cdp-gen generate browser.json js.json out Alpha,Beta
  wrote glue: cdp_json.ml, cdp_command.ml, cdp_event.ml, cdp_envelope.ml
  generated cdp_base.ml: 2 sealed alias modules
  generated cdp_alpha(_types).ml: 1 types, 0 commands, 0 events
  generated cdp_beta(_types).ml: 1 types, 0 commands, 0 events
  generated cdp.ml index (2 domains, protocol unknown)
  $ ls out
  cdp.ml
  cdp_alpha.ml
  cdp_alpha_types.ml
  cdp_base.ml
  cdp_beta.ml
  cdp_beta_types.ml
  cdp_command.ml
  cdp_envelope.ml
  cdp_event.ml
  cdp_json.ml
  cdp_my_helper.ml

  $ cdp-gen generate browser.json js.json out Beta
  removed stale cdp_alpha.ml
  removed stale cdp_alpha_types.ml
  wrote glue: cdp_json.ml, cdp_command.ml, cdp_event.ml, cdp_envelope.ml
  generated cdp_base.ml: 1 sealed alias modules
  generated cdp_beta(_types).ml: 1 types, 0 commands, 0 events
  generated cdp.ml index (1 domains, protocol unknown)
  $ ls out
  cdp.ml
  cdp_base.ml
  cdp_beta.ml
  cdp_beta_types.ml
  cdp_command.ml
  cdp_envelope.ml
  cdp_event.ml
  cdp_json.ml
  cdp_my_helper.ml
  $ grep -c Alpha out/cdp_base.ml
  0
  [1]
  $ cat out/cdp_my_helper.ml
  (* my own helper *)

A failing run removes nothing: validation happens before any file is touched.

  $ cat > bad.json << 'EOF'
  > {"domains":[{"domain":"Beta","types":[{"id":"BetaId","type":"string"},{"id":"BetaId","type":"string"}]}]}
  > EOF
  $ cdp-gen generate bad.json js.json out Beta
  cdp-gen: type Beta.BetaId is defined twice
  [1]
  $ ls out
  cdp.ml
  cdp_base.ml
  cdp_beta.ml
  cdp_beta_types.ml
  cdp_command.ml
  cdp_envelope.ml
  cdp_event.ml
  cdp_json.ml
  cdp_my_helper.ml
