(lang dune 3.7)

(name camlisp)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name camlisp)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune ppx_inline_test ppx_assert)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
