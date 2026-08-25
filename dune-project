(lang dune 3.16)

(name cdp)

(generate_opam_files true)

(authors
 "Vitalii Shapoval <vytalych@gmail.com>"
 "Ahrefs <github@ahrefs.com>")

(maintainers
 "Vitalii Shapoval <vytalych@gmail.com>"
 "Ahrefs <github@ahrefs.com>")

(license "MIT AND BSD-3-Clause")

(homepage "https://github.com/ahrefs/ocaml-cdp")

(bug_reports "https://github.com/ahrefs/ocaml-cdp/issues")

(source
 (github ahrefs/ocaml-cdp))

(package
 (name cdp)
 (synopsis "Chrome DevTools Protocol types for OCaml")
 (description
  "Typed commands, events, and JSON codecs for the Chrome DevTools Protocol, generated from the official protocol definitions. Transport-agnostic: pair it with cdp-lwt or bring your own transport.")
 (tags
  ("org:ahrefs" chrome devtools cdp browser))
 (depends
  (ocaml (>= "4.14"))
  (yojson (>= "1.6.0"))
  (jsonkit
   (and
    (>= "1.0.0")
    (< "2.0.0")))
  (ppx_deriving (>= "6.0"))
  (ocamlformat
   (and
    (= "0.29.0")
    :with-dev-setup))
  (ocaml-lsp-server :with-dev-setup)))

(package
 (name cdp-gen)
 (synopsis "Generator producing the cdp library from CDP protocol JSON")
 (description
  "Command-line generator behind the cdp package: fetches a devtools-protocol snapshot and emits typed OCaml modules with JSON codecs for the selected domains.")
 (tags
  ("org:ahrefs" chrome devtools cdp codegen))
 (depends
  (ocaml (>= "4.14"))
  (yojson (>= "1.6.0"))))

(package
 (name cdp-lwt)
 (synopsis "Lwt transport for the cdp library over libcurl WebSockets")
 (description
  "Lwt client for the Chrome DevTools Protocol: launches or attaches to a Chrome and drives it over libcurl WebSockets, using the typed commands and events from the cdp package.")
 (tags
  ("org:ahrefs" chrome devtools cdp lwt))
 (depends
  (ocaml (>= "4.14"))
  (cdp (= :version))
  curl
  curl_lwt
  (lwt (>= "5.10"))
  (lwt_ppx (>= "5.9"))))
