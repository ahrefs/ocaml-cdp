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
