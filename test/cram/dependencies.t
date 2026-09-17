Generating a subset of domains: every domain a selection points at through
$ref, directly or through another domain, is reported in one message, so the
user adds the whole set in one run.

  $ cat > browser.json << 'EOF'
  > {"domains":[
  >   {"domain":"Alpha","types":[
  >     {"id":"Node","type":"object","properties":[{"name":"thing","$ref":"Beta.Thing"}]}
  >   ]},
  >   {"domain":"Beta","types":[
  >     {"id":"Thing","type":"object","properties":[{"name":"x","type":"integer"}]}
  >   ],"commands":[
  >     {"name":"poke","parameters":[{"name":"id","$ref":"Gamma.Id"}]}
  >   ]},
  >   {"domain":"Gamma","types":[{"id":"Id","type":"string"}]},
  >   {"domain":"Delta","types":[{"id":"Unused","type":"string"}]}
  > ]}
  > EOF
  $ echo '{"domains":[]}' > js.json
  $ mkdir out && cdp-gen generate browser.json js.json out Alpha
  cdp-gen: Alpha also needs Beta,Gamma; add them to the domain list
  [1]
  $ ls out

  $ cdp-gen generate browser.json js.json out Alpha,Gamma
  cdp-gen: the selected domains also need Beta; add them to the domain list
  [1]

A closed selection generates; a domain nobody points at is not pulled in.

  $ cdp-gen generate browser.json js.json out Alpha,Beta,Gamma | tail -1
  generated cdp.ml index (3 domains, protocol unknown)
  $ ls out | grep -c delta
  0
  [1]
