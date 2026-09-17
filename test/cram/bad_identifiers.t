Names from the protocol become OCaml code; anything the sanitizers cannot
turn into a valid identifier is refused with the owner's name, instead of
emitting code that fails to compile.

  $ cat > browser.json << 'EOF'
  > {"domains":[{"domain":"Demo","types":[
  >   {"id":"Point","type":"object","properties":[{"name":"x coordinate","type":"integer"}]}
  > ]}]}
  > EOF
  $ echo '{"domains":[]}' > js.json
  $ mkdir out && cdp-gen generate browser.json js.json out all
  cdp-gen: Demo.Point: name "x coordinate" is not a plain identifier
  [1]
  $ ls out

Two enum values that collapse to the same constructor would produce a
variant with a duplicate case.

  $ cat > browser.json << 'EOF'
  > {"domains":[{"domain":"Demo","types":[
  >   {"id":"Mood","type":"string","enum":["very-sad","very_sad"]}
  > ]}]}
  > EOF
  $ cdp-gen generate browser.json js.json out all
  cdp-gen: Demo.Mood: enum values "very-sad" and "very_sad" both become the constructor Very_sad
  [1]

An empty enum value has no possible constructor.

  $ cat > browser.json << 'EOF'
  > {"domains":[{"domain":"Demo","types":[
  >   {"id":"Mood","type":"string","enum":[""]}
  > ]}]}
  > EOF
  $ cdp-gen generate browser.json js.json out all
  cdp-gen: Demo.Mood: an enum value is empty
  [1]

Two fields of one record that become the same OCaml label would make the
compiler reject the generated file; the generator refuses first. Command
parameters and returns are checked the same way.

  $ cat > browser.json << 'EOF'
  > {"domains":[{"domain":"Demo","types":[
  >   {"id":"Request","type":"object","properties":[
  >     {"name":"requestURL","type":"string"},
  >     {"name":"requestUrl","type":"string"}
  >   ]}
  > ]}]}
  > EOF
  $ cdp-gen generate browser.json js.json out all
  cdp-gen: Demo.Request: fields "requestURL" and "requestUrl" both become request_url
  [1]

  $ cat > browser.json << 'EOF'
  > {"domains":[{"domain":"Demo","commands":[
  >   {"name":"fetch","returns":[
  >     {"name":"fooBar","type":"string"},
  >     {"name":"foo_bar","type":"string"}
  >   ]}
  > ]}]}
  > EOF
  $ cdp-gen generate browser.json js.json out all
  cdp-gen: Demo.fetch: fields "fooBar" and "foo_bar" both become foo_bar
  [1]
  $ ls out
