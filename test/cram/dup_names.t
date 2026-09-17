A repeated domain name or type id would silently overwrite its sibling in
the output; the generator refuses instead.

  $ cat > two_domains.json << 'EOF'
  > {"domains":[{"domain":"Demo","types":[]},{"domain":"Demo","types":[]}]}
  > EOF
  $ echo '{"domains":[]}' > empty.json
  $ mkdir out && cdp-gen generate two_domains.json empty.json out all
  cdp-gen: domain Demo is defined twice
  [1]
  $ ls out

  $ cat > two_types.json << 'EOF'
  > {"domains":[{"domain":"Demo","types":[
  >   {"id":"Mood","type":"string"},
  >   {"id":"Mood","type":"object","properties":[{"name":"x","type":"integer"}]}
  > ]}]}
  > EOF
  $ cdp-gen generate two_types.json empty.json out all
  cdp-gen: type Demo.Mood is defined twice
  [1]
  $ ls out

A command or event defined twice would get two modules for one wire name,
the second one under the _command / _event fallback; the generator refuses.

  $ cat > two_commands.json << 'EOF'
  > {"domains":[{"domain":"Demo","commands":[{"name":"poke"},{"name":"poke"}]}]}
  > EOF
  $ cdp-gen generate two_commands.json empty.json out all
  cdp-gen: command Demo.poke is defined twice
  [1]

  $ cat > two_events.json << 'EOF'
  > {"domains":[{"domain":"Demo","events":[{"name":"ping"},{"name":"ping"}]}]}
  > EOF
  $ cdp-gen generate two_events.json empty.json out all
  cdp-gen: event Demo.ping is defined twice
  [1]
  $ ls out
