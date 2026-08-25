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
 (depends
  (ocaml (>= "4.14"))
  yojson
  melange-json-native
  ppx_deriving
  (ocamlformat
   (and
    (= "0.29.0")
    :with-test))
  (ocaml-lsp-server :with-dev-setup)))

(package
 (name cdp-gen)
 (synopsis "Generator producing the cdp library from CDP protocol JSON")
 (depends
  (ocaml (>= "4.14"))
  yojson))

(package
 (name cdp-lwt)
 (synopsis "Lwt transport for the cdp library over libcurl WebSockets")
 (depends
  (ocaml (>= "4.14"))
  (cdp (= :version))
  curl
  curl_lwt
  lwt
  lwt_ppx))
