A $ref to a type that exists nowhere fails with the referrer's name,
instead of generating code that breaks at compile time. The output
directory stays empty: nothing is written before validation passes.

  $ cat > browser.json << 'EOF'
  > {"domains":[{"domain":"Demo","types":[
  >   {"id":"Thing","type":"object","properties":[{"name":"other","$ref":"Demo.Missing"}]}
  > ]}]}
  > EOF
  $ echo '{"domains":[]}' > js.json
  $ mkdir out && cdp-gen generate browser.json js.json out Demo
  cdp-gen: Demo.Thing references Demo.Missing, which does not exist
  [1]
  $ ls out

A ref from a command's parameters is checked too.

  $ cat > browser.json << 'EOF'
  > {"domains":[{"domain":"Demo","commands":[
  >   {"name":"poke","parameters":[{"name":"where","$ref":"Elsewhere.Spot"}]}
  > ]}]}
  > EOF
  $ cdp-gen generate browser.json js.json out Demo
  cdp-gen: Demo.poke references Elsewhere.Spot, which does not exist
  [1]
