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
