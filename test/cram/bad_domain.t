A domain name from the protocol JSON becomes output file names and module
names, so anything beyond letters and digits is rejected — a hostile name
could otherwise write outside the output directory.

  $ cat > browser.json << 'EOF'
  > {"domains":[{"domain":"../Evil","types":[]}]}
  > EOF
  $ echo '{"domains":[]}' > js.json
  $ mkdir out && cdp-gen generate browser.json js.json out all
  cdp-gen: domain name "../Evil" is not a plain identifier (letters and digits only)
  [1]
  $ ls out
