One file per domain means the cross-domain graph of record and enum types must
have no cycle. A cycle stops generation with the path and writes nothing; the
CLI has no flag to merge the domains, so a future roll that adds such a loop
needs a generator change.

  $ cat > browser.json << 'EOF'
  > {"domains":[
  >   {"domain":"Alpha","types":[
  >     {"id":"Node","type":"object","properties":[{"name":"leaf","$ref":"Beta.Leaf","optional":true}]}
  >   ]},
  >   {"domain":"Beta","types":[
  >     {"id":"Leaf","type":"object","properties":[{"name":"parent","$ref":"Alpha.Node","optional":true}]}
  >   ]}
  > ]}
  > EOF
  $ echo '{"domains":[]}' > js.json
  $ mkdir out && cdp-gen generate browser.json js.json out Alpha,Beta
  cdp-gen: type-level cycle between domains: Alpha -> Beta -> Alpha. Sealed-alias routing through Base cannot break it; the domains in the cycle must be merged into one compilation unit.
  [1]
  $ ls out

A loop that goes through a primitive alias is fine: the alias lives in
cdp_base.ml, which depends on nothing.

  $ cat > browser.json << 'EOF'
  > {"domains":[
  >   {"domain":"Alpha","types":[
  >     {"id":"AlphaId","type":"string"},
  >     {"id":"Node","type":"object","properties":[{"name":"leaf","$ref":"Beta.Leaf"}]}
  >   ]},
  >   {"domain":"Beta","types":[
  >     {"id":"Leaf","type":"object","properties":[{"name":"owner","$ref":"Alpha.AlphaId"}]}
  >   ]}
  > ]}
  > EOF
  $ cdp-gen generate browser.json js.json out Alpha,Beta > /dev/null
  $ ls out
  cdp.ml
  cdp_alpha.ml
  cdp_alpha_types.ml
  cdp_base.ml
  cdp_beta.ml
  cdp_beta_types.ml
