Malformed input fails with a clean cdp-gen message and exit 1, not a raw
uncaught exception.

  $ echo '{broken' > browser.json
  $ echo '{"domains":[]}' > js.json
  $ mkdir out && cdp-gen generate browser.json js.json out all
  cdp-gen: invalid protocol JSON: Line 2, bytes -1-0:
  Unexpected end of input
  [1]

  $ cdp-gen generate does_not_exist.json js.json out all
  cdp-gen: does_not_exist.json: No such file or directory
  [1]

A structurally wrong protocol (a type without an id) is also a clean error.

  $ cat > browser.json << 'EOF'
  > {"domains":[{"domain":"Demo","types":[{"type":"string"}]}]}
  > EOF
  $ cdp-gen generate browser.json js.json out all
  cdp-gen: unexpected protocol shape: Expected string, got null
  [1]
  $ ls out

A non-boolean "optional" would silently flip a field to required.

  $ cat > browser.json << 'EOF2'
  > {"domains":[{"domain":"Demo","types":[
  >   {"id":"Point","type":"object","properties":[{"name":"x","type":"integer","optional":"true"}]}
  > ]}]}
  > EOF2
  $ cdp-gen generate browser.json js.json out all
  cdp-gen: field "optional" must be a boolean, got "true"
  [1]

A REVISION file whose content is not a revision id is refused — it would be
stamped into every generated header.

  $ cat > browser.json << 'EOF2'
  > {"domains":[{"domain":"Demo","types":[]}]}
  > EOF2
  $ printf 'r123 *) let boom = ' > REVISION
  $ cdp-gen generate browser.json js.json out all
  cdp-gen: REVISION next to the protocol JSON holds "r123 *) let boom =", which is not a revision id
  [1]
  $ ls out
