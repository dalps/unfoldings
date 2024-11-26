(lang dune 3.5)

(name unfoldings)

(generate_opam_files true)

(source
 (github dalps/unfoldings))

(authors "Federico Dal Pio Luogo")

(maintainers "Federico Dal Pio Luogo")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name unfoldings)
 (synopsis "An OCaml framework to reason about Petri nets and their unfolding")
 (description "An OCaml framework to reason about Petri nets and their unfolding")
 (depends ocaml dune)
 (tags
  (petrinet unfolding modelchecking verification)))

(using menhir 2.1)
