(lang dune 2.9)
(name pallvm)
(using menhir 2.1)
(generate_opam_files true)
(package
  (name pallvm)
  (depends
    menhir
    llvm
  )
)
