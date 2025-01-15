## v0.4.3

- No longer rely on `joo_global_object` (#7)

## v0.4.2

- Separate tests by library and remove `lwt` dependency from `promise_jsoo`
  tests.

## v0.4.1

- Require `lwt` when running `promise_jsoo` tests.

## v0.4.0

- Add `promise_jsoo_lwt` library and `Promise_lwt` module to convert between JS
  promises and Lwt promises
- Change internal representation to `Ojs.t` and make `Promise.t` abstract.
- Remove `Promise.void` type (can now be expressed as `unit Promise.t`).
- Add a `Make` functor to create modules with a custom type representation.
- Give the `error` type a public `Ojs.t` type representation.
- Remove js_of_ocaml-ppx dependency.

## v0.3.1

- Reduce the size of .js binaries produced by using this library by not linking
  the ppx toolchain. (#3)

## v0.3.0

- Fix `Promise.Array.find_map` and `Promise.List.find_map` raising
  `Assert_failure` (#1)
- Stop calling the function once `Promise.Array.find_map` and
  `Promise.List.find_map` find a value (#1)

## v0.2.0

- Add gen_js_api conversion function for `error` type
- Fix gen_js_api conversion function for promise type

## v0.1.0

- Initial release
