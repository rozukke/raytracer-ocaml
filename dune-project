(lang dune 3.19)

(name raytracer)

(generate_opam_files true)

(source
 (github rozukke/raytracer))

(authors "Artemis Rosman <73006620+rozukke@users.noreply.github.com>")

; (maintainers "Maintainer Name <guy@example.com>")

(license LICENSE)

; (documentation https://url/to/documentation)

(package
 (name raytracer)
 (synopsis "Raytracer in a weekend with OCaml.")
 (description "A way for me to quickly get used to the language by implementing 'Raytracing in a Weekend'.")
 (depends ocaml base)
 (tags (raytracing))
 )

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
