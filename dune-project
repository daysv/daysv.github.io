(lang dune 3.18)

(name yocaml_blog)

(version dev)

(generate_opam_files true)

(using mdx 0.4)

(maintenance_intent "(latest)")

(source
 (github daysv/yocaml-blog))

(license GPL-3.0-or-later)

(authors "daysv <days_v@qq.com>")

(maintainers "daysv <days_v@qq.com>")

(package
 (name yocaml_blog)
 (synopsis "blog engine written in OCaml")
 (depends
  (ocaml
   (>= 5.1.1))
  ppx_expect
  (mdx
   (and
    :with-test
    (>= 2.5.0)))
  (lwt
   (>= 5.7.0))
  (mimic
   (>= 0.0.9))
  (cstruct
   (>= 6.2.0))
  (git-kv
   (>= 0.2.0))
  (git-net
   (>= 0.2.0))
  (mirage-clock
   (>= 4.2.0))
  (mirage-crypto-rng
   (>= 1.2.0))
  (yocaml
   (= :version))
  (yocaml_runtime
   (= :version))))
