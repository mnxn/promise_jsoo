(lang dune 2.7)

(name promise_jsoo)

(license MIT)

(maintainers "Max Lantas <mnxn.dev@outlook.com>")

(authors "Max Lantas <mnxn.dev@outlook.com>")

(source
 (github mnxn/promise_jsoo))

(generate_opam_files true)

(implicit_transitive_deps false)

(package
 (name promise_jsoo)
 (synopsis "Js_of_ocaml bindings to JS Promises with supplemental functions")
 (depends
  js_of_ocaml
  gen_js_api
  (webtest :with-test)
  (webtest-js :with-test)))
