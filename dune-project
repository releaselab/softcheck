(lang dune 2.8)

(name softcheck)

(generate_opam_files true)

(source
 (uri "git+https://gitlab.com/joaosreis/softcheck.git"))

(bug_reports "https://gitlab.com/joaosreis/softcheck/issues")

(homepage "https://gitlab.com/joaosreis/softcheck")

(license MIT)

(authors "Jo\195\163o Santos Reis <joao.reis@ubi.pt>")

(maintainers "Jo\195\163o Santos Reis <joao.reis@ubi.pt>")

(package
 (name softcheck)
 (synopsis "A modular static analysis platform")
 (depends
  (dune :build)
  base
  fix
  bitv
  ocamlgraph
  ppx_string
  ppx_deriving))
