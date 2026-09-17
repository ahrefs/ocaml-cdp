A command module includes the domain types. An inline enum hoisted under a
field name that is also a domain type would hide that type, and a $ref to it
in the same module would bind to the enum. The hoisted enum gets the module
name as prefix instead.

  $ cat > browser.json << 'EOF'
  > {"domains":[{"domain":"Demo",
  >   "types":[{"id":"Frame","type":"string","enum":["main","child"]}],
  >   "commands":[{"name":"paint","parameters":[
  >     {"name":"frame","type":"string","enum":["front","back"]},
  >     {"name":"target","$ref":"Frame"}
  >   ]}]
  > }]}
  > EOF
  $ echo '{"domains":[]}' > js.json
  $ mkdir out && cdp-gen generate browser.json js.json out Demo > /dev/null
  $ grep -A9 'module Paint' out/cdp_demo.ml
  module Paint = struct
  let name = "Demo.paint"
  
  type paint_frame =
    | Front [@json.name "front"]
    | Back [@json.name "back"]
    | Other of Cdp_json.unknown [@json.catch_all]
  [@@compact_variants]
  [@@deriving json, show, eq]
  


  $ grep 'target :' out/cdp_demo.ml
    target : frame [@key "target"];

Without a clash the hoisted enum keeps the plain field name.

  $ cat > browser.json << 'EOF'
  > {"domains":[{"domain":"Demo",
  >   "types":[{"id":"Frame","type":"string","enum":["main","child"]}],
  >   "commands":[{"name":"paint","parameters":[
  >     {"name":"side","type":"string","enum":["front","back"]}
  >   ]}]
  > }]}
  > EOF
  $ cdp-gen generate browser.json js.json out Demo > /dev/null
  $ grep -c 'type side =' out/cdp_demo.ml
  1
