(lang dune 2.7)

(name promise_jsoo)

(license MIT)

(maintainers "Max Lantas <mnxndev@outlook.com>")

(authors "Max Lantas <mnxndev@outlook.com>")

(source
 (github mnxn/promise_jsoo))

(generate_opam_files true)

(implicit_transitive_deps false)

(package
 (name promise_jsoo)
 (synopsis "Js_of_ocaml bindings to JS Promises with supplemental functions")
 (depends
  (ocaml
   (>= 4.08))
  js_of_ocaml
  js_of_ocaml-ppx
  gen_js_api
  (webtest :with-test)
  (webtest-js :with-test)
  (conf-npm :with-test)))

(package
 (name promise_jsoo_lwt)
 (synopsis "Js_of_ocaml bindings to JS Promises as Lwt Promises")
 (depends
  (ocaml
   (>= 4.08))
  promise_jsoo
  lwt
  gen_js_api
  (webtest :with-test)
  (webtest-js :with-test)
  (conf-npm :with-test)))
