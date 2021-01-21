# Change Log

## Unreleased

-   Use `promise Js.t` type for compatibility with Js_of_ocaml bindings
-   Add `promise_jsoo_lwt` library and `Promise_lwt` module to convert between
    JS promises and Lwt promises

## 0.3.1

-   Reduce the size of .js binaries produced by using this library by not
    linking the ppx toolchain. (#3)

## 0.3.0

-   Fix `Promise.Array.find_map` and `Promise.List.find_map` raising
    `Assert_failure` (#1)
-   Stop calling the function once `Promise.Array.find_map` and
    `Promise.List.find_map` find a value (#1)

## 0.2.0

-   Add gen_js_api conversion function for `error` type
-   Fix gen_js_api conversion function for promise type

## 0.1.0

-   Initial release
