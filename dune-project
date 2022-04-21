(lang dune 2.8)

(name softcheck)

(generate_opam_files true)

(source
 (uri "git+https://github.com/joaosreis/softcheck.git"))

(bug_reports "https://github.com/joaosreis/softcheck/issues")

(homepage "https://github.com/joaosreis/softcheck")

(license MIT)

(authors "Jo\195\163o Santos Reis <joaosreis@outlook.pt>")

(maintainers "Jo\195\163o Santos Reis <joaosreis@outlook.pt>")

(package
 (name softcheck)
 (synopsis "A modular static analysis platform")
 (depends
  (dune :build)
  (core
   (>= v0.15.0))
  (core_unix
   (>= v0.15.0))
  fix
  bitv
  ocamlgraph
  ppx_deriving))
