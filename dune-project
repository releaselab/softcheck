(lang dune 2.8)

(name softcheck)

(generate_opam_files true)

(source
 (uri "git+https://github.com/releaselab/softcheck.git"))

(bug_reports "https://github.com/releaselab/softcheck/issues")

(homepage "https://github.com/releaselab/softcheck")

(license MIT)

(authors "Jo\195\163o Santos Reis <joao.reis@ubi.pt>")

(maintainers "Jo\195\163o Santos Reis <joao.reis@ubi.pt>")

(package
 (name softcheck)
 (synopsis "A modular static analysis platform")
 (depends
  (dune :build)
  core
  fix
  bitv
  ocamlgraph
  ppx_string
  ppx_deriving))
