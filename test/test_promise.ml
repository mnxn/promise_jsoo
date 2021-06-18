open Webtest.Suite

open
  [%js:
  val setTimeout : (unit -> unit) -> int -> unit [@@js.global]

  val eval : string -> Ojs.t [@@js.global]

  val finally_method : Ojs.t option [@@js.global "Promise.prototype.finally"]]

let pass finish () = finish Async.noop

let fail finish () = finish (fun () -> assert_true false)

let timeout f = setTimeout f 1000

let valid promise = Option.is_some @@ [%js.to: 'a option] promise

module Tests (P : Promise.S) = struct
  let timeout_promise finish =
    P.make (fun ~resolve:_ ~reject:_ -> timeout (fail finish))

  module CoreFunctions = struct
    let test_make () =
      let promise = P.make (fun ~resolve:_ ~reject:_ -> ()) in
      let js = [%js.of: 'a P.t] promise in
      assert_true (valid js)

    let test_resolve () =
      let promise = P.resolve () in
      let js = [%js.of: unit P.t] promise in
      assert_true (valid js)

    let test_reject () =
      let promise = P.reject @@ [%js.of: unit] () in
      let js = [%js.of: 'a P.t] promise in
      assert_true (valid js)

    let suite =
      "CoreFunctions"
      >::: [ "test_make" >:: test_make
           ; "test_resolve" >:: test_resolve
           ; "test_reject" >:: test_reject
           ]
  end

  module InstanceMethods = struct
    let test_then_fulfilled finish =
      let promise = P.resolve 1 in
      let fulfilled value =
        finish (fun () -> assert_equal value 1);
        P.resolve ()
      in
      let (_ : unit P.t) = P.then_ promise ~fulfilled in
      timeout (fail finish)

    let test_then_rejected finish =
      let promise = P.reject @@ [%js.of: int] 2 in
      let fulfilled = P.resolve in
      let rejected (reason : P.error) =
        let reason = [%js.to: int] reason in
        finish (fun () -> assert_equal reason 2);
        P.resolve ()
      in
      let (_ : unit P.t) = P.then_ promise ~fulfilled ~rejected in
      timeout (fail finish)

    let test_catch finish =
      let promise = P.reject @@ [%js.of: int] 3 in
      let rejected (reason : P.error) =
        let reason = [%js.to: int] reason in
        finish (fun () -> assert_equal reason 3);
        P.resolve ()
      in
      let (_ : unit P.t) = P.catch promise ~rejected in
      timeout (fail finish)

    let test_finally_resolved finish =
      let promise = P.resolve () in
      let (_ : unit P.t) = P.finally promise ~f:(pass finish) in
      timeout (fail finish)

    let test_finally_rejected finish =
      let promise = P.reject @@ [%js.of: unit] () in
      let rejected (_ : P.error) = P.return () in
      let (_ : unit P.t) =
        P.finally promise ~f:(pass finish) |> P.catch ~rejected
      in
      timeout (fail finish)

    let test_make_resolve finish =
      let promise = P.make (fun ~resolve ~reject:_ -> resolve 4) in
      let fulfilled value =
        finish (fun () -> assert_equal value 4);
        P.resolve ()
      in
      let (_ : unit P.t) = P.then_ promise ~fulfilled in
      timeout (fail finish)

    let test_make_reject finish =
      let promise =
        P.make (fun ~resolve:_ ~reject -> reject @@ [%js.of: int] 5)
      in
      let rejected (reason : P.error) =
        let reason = [%js.to: int] reason in
        finish (fun () -> assert_equal reason 5);
        P.resolve ()
      in
      let (_ : unit P.t) = P.catch promise ~rejected in
      timeout (fail finish)

    let finally_tests =
      match finally_method with
      | None   -> []
      | Some _ ->
        [ "test_finally_resolved" >:~ test_finally_resolved
        ; "test_finally_rejected" >:~ test_finally_rejected
        ]

    let suite =
      "InstanceMethods"
      >::: [ "test_then_fulfilled" >:~ test_then_fulfilled
           ; "test_then_rejected" >:~ test_then_rejected
           ; "test_catch" >:~ test_catch
           ; "test_make_resolve" >:~ test_make_resolve
           ; "test_make_reject" >:~ test_make_reject
           ]
           @ finally_tests
  end

  module StaticMethods = struct
    let test_all finish =
      let promises = [| P.resolve 1; P.resolve 2; P.resolve 3 |] in
      let fulfilled value =
        finish (fun () -> assert_equal value [| 1; 2; 3 |]);
        P.resolve ()
      in
      let result = P.all promises in
      let (_ : unit P.t) = result |> P.then_ ~fulfilled in
      timeout (fail finish)

    let test_all_list finish =
      let promises = [ P.resolve 1; P.resolve 2; P.resolve 3 ] in
      let fulfilled value =
        finish (fun () -> assert_equal value [ 1; 2; 3 ]);
        P.resolve ()
      in
      let result = P.all_list promises in
      let (_ : unit P.t) = result |> P.then_ ~fulfilled in
      timeout (fail finish)

    let test_all2 finish =
      let promises = (P.resolve 1, P.resolve "2") in
      let fulfilled value =
        finish (fun () -> assert_equal value (1, "2"));
        P.resolve ()
      in
      let result = P.all2 promises in
      let (_ : unit P.t) = result |> P.then_ ~fulfilled in
      timeout (fail finish)

    let test_all3 finish =
      let promises = (P.resolve true, P.resolve 2, P.resolve "3") in
      let fulfilled value =
        finish (fun () -> assert_equal value (true, 2, "3"));
        P.resolve ()
      in
      let result = P.all3 promises in
      let (_ : unit P.t) = result |> P.then_ ~fulfilled in
      timeout (fail finish)

    let test_race finish =
      let promises = [| P.resolve 1; timeout_promise finish |] in
      let fulfilled value =
        finish (fun () -> assert_equal value 1);
        P.resolve ()
      in
      let result = P.race promises in
      let (_ : unit P.t) = result |> P.then_ ~fulfilled in
      timeout (fail finish)

    let test_race_list finish =
      let promises = [ P.resolve 1; timeout_promise finish ] in
      let fulfilled value =
        finish (fun () -> assert_equal value 1);
        P.resolve ()
      in
      let result = P.race_list promises in
      let (_ : unit P.t) = result |> P.then_ ~fulfilled in
      timeout (fail finish)

    let suite =
      "StaticMethods"
      >::: [ "test_all" >:~ test_all
           ; "test_all_list" >:~ test_all_list
           ; "test_all2" >:~ test_all2
           ; "test_all3" >:~ test_all3
           ; "test_race" >:~ test_race
           ; "test_race_list" >:~ test_race_list
           ]
  end

  module Supplemental = struct
    let test_return finish =
      let promise = P.return 5 in
      let fulfilled value =
        finish (fun () -> assert_equal value 5);
        P.resolve ()
      in
      let (_ : unit P.t) = promise |> P.then_ ~fulfilled in
      timeout (fail finish)

    let test_map finish =
      let promise = P.return 2 in
      let fulfilled value =
        finish (fun () -> assert_equal value 6);
        P.return ()
      in
      let result = P.map (fun x -> x * 3) promise in
      let (_ : unit P.t) = result |> P.then_ ~fulfilled in
      timeout (fail finish)

    let test_bind finish =
      let promise = P.return 12 in
      let fulfilled value =
        finish (fun () -> assert_equal value 3);
        P.return ()
      in
      let result = P.bind (fun x -> P.return (x / 4)) promise in
      let (_ : unit P.t) = result |> P.then_ ~fulfilled in
      timeout (fail finish)

    module SupplementalSyntax = struct
      let test_map_op finish =
        let promise = P.return 2 in
        let fulfilled value =
          finish (fun () -> assert_equal value 6);
          P.return ()
        in
        let result =
          let open P.Syntax in
          promise >>| fun x -> x * 3
        in
        let (_ : unit P.t) = result |> P.then_ ~fulfilled in
        timeout (fail finish)

      let test_bind_op finish =
        let promise = P.return 12 in
        let fulfilled value =
          finish (fun () -> assert_equal value 3);
          P.return ()
        in
        let result =
          let open P.Syntax in
          promise >>= fun x -> P.return (x / 4)
        in
        let (_ : unit P.t) = result |> P.then_ ~fulfilled in
        timeout (fail finish)

      let test_map_let_op finish =
        let promise = P.return 2 in
        let fulfilled value =
          finish (fun () -> assert_equal value 6);
          P.return ()
        in
        let result =
          let open P.Syntax in
          let+ x = promise in
          x * 3
        in
        let (_ : unit P.t) = result |> P.then_ ~fulfilled in
        timeout (fail finish)

      let test_bind_let_op finish =
        let promise = P.return 12 in
        let fulfilled value =
          finish (fun () -> assert_equal value 3);
          P.return ()
        in
        let result =
          let open P.Syntax in
          let* x = promise in
          P.return (x / 4)
        in
        let (_ : unit P.t) = result |> P.then_ ~fulfilled in
        timeout (fail finish)

      let suite =
        "Syntax"
        >::: [ "test_map_op" >:~ test_map_op
             ; "test_bind_op" >:~ test_bind_op
             ; "test_map_let_op" >:~ test_map_let_op
             ; "test_bind_let_op" >:~ test_bind_let_op
             ]
    end

    module SupplementalOption = struct
      let test_return finish =
        let promise = P.Option.return 5 in
        let fulfilled value =
          finish (fun () -> assert_equal value (Some 5));
          P.return ()
        in
        let (_ : unit P.t) = promise |> P.then_ ~fulfilled in
        timeout (fail finish)

      let test_map_some finish =
        let promise = P.Option.return 2 in
        let fulfilled value =
          finish (fun () -> assert_equal value (Some 6));
          P.return ()
        in
        let result = P.Option.map (fun x -> x * 3) promise in
        let (_ : unit P.t) = result |> P.then_ ~fulfilled in
        timeout (fail finish)

      let test_map_none finish =
        let promise = P.return None in
        let fulfilled value =
          finish (fun () -> assert_equal value None);
          P.return ()
        in
        let result = P.Option.map (fun x -> x * 3) promise in
        let (_ : unit P.t) = result |> P.then_ ~fulfilled in
        timeout (fail finish)

      let test_bind_some finish =
        let promise = P.Option.return 12 in
        let fulfilled value =
          finish (fun () -> assert_equal value (Some 3));
          P.return ()
        in
        let result = P.Option.bind (fun x -> P.Option.return (x / 4)) promise in
        let (_ : unit P.t) = result |> P.then_ ~fulfilled in
        timeout (fail finish)

      let test_bind_none finish =
        let promise = P.return None in
        let fulfilled value =
          finish (fun () -> assert_equal value None);
          P.return ()
        in
        let result = P.Option.bind (fun x -> P.Option.return (x / 4)) promise in
        let (_ : unit P.t) = result |> P.then_ ~fulfilled in
        timeout (fail finish)

      let test_iter_some finish =
        let promise = P.Option.return 5 in
        let fulfilled () =
          fail finish ();
          P.return ()
        in
        let f value = finish (fun () -> assert_equal value 5) in
        let result = P.Option.iter f promise in
        let (_ : unit P.t) = result |> P.then_ ~fulfilled in
        timeout (fail finish)

      let test_iter_none finish =
        let promise = P.return None in
        let fulfilled () =
          pass finish ();
          P.resolve ()
        in
        let result = P.Option.iter (fail finish) promise in
        let (_ : unit P.t) = result |> P.then_ ~fulfilled in
        timeout (fail finish)

      let suite =
        "Option"
        >::: [ "test_return" >:~ test_return
             ; "test_map_some" >:~ test_map_some
             ; "test_bind_some" >:~ test_bind_some
             ; "test_map_none" >:~ test_map_none
             ; "test_bind_none" >:~ test_bind_none
             ; "test_iter_some" >:~ test_iter_some
             ; "test_iter_none" >:~ test_iter_none
             ]
    end

    module SupplementalResult = struct
      let test_from_catch_fulfilled finish =
        let promise = P.resolve 1 in
        let fulfilled value =
          finish (fun () -> assert_equal value (Ok 1));
          P.return ()
        in
        let result = P.Result.from_catch promise in
        let (_ : unit P.t) = result |> P.then_ ~fulfilled in
        timeout (fail finish)

      let test_from_catch_rejected finish =
        let promise = P.reject @@ [%js.of: string] "error" in
        let fulfilled (value : ('a, P.error) Result.t) =
          let expected = function
            | Ok _         -> false
            | Error reason -> [%js.to: string] reason = "error"
          in
          finish (fun () -> assert_true (expected value));
          P.return ()
        in
        let result = P.Result.from_catch promise in
        let (_ : unit P.t) = result |> P.then_ ~fulfilled in
        timeout (fail finish)

      let test_return finish =
        let promise = P.Result.return 5 in
        let fulfilled value =
          finish (fun () -> assert_equal value (Ok 5));
          P.return ()
        in
        let (_ : unit P.t) = promise |> P.then_ ~fulfilled in
        timeout (fail finish)

      let test_map_ok finish =
        let promise = P.Result.return 2 in
        let fulfilled value =
          finish (fun () -> assert_equal value (Ok 6));
          P.return ()
        in
        let result = P.Result.map (fun x -> x * 3) promise in
        let (_ : unit P.t) = result |> P.then_ ~fulfilled in
        timeout (fail finish)

      let test_map_error finish =
        let promise = P.return (Result.Error "error") in
        let fulfilled value =
          finish (fun () -> assert_equal value (Result.Error "error"));
          P.return ()
        in
        let result = P.Result.map (fun x -> x * 3) promise in
        let (_ : unit P.t) = result |> P.then_ ~fulfilled in
        timeout (fail finish)

      let test_bind_ok finish =
        let promise = P.Result.return 12 in
        let fulfilled value =
          finish (fun () -> assert_equal value (Ok 3));
          P.return ()
        in
        let result = P.Result.bind (fun x -> P.Result.return (x / 4)) promise in
        let (_ : unit P.t) = result |> P.then_ ~fulfilled in
        timeout (fail finish)

      let test_bind_error finish =
        let promise = P.return (Result.Error "error") in
        let fulfilled value =
          finish (fun () -> assert_equal value (Result.error "error"));
          P.return ()
        in
        let result = P.Result.bind (fun x -> P.Result.return (x / 4)) promise in
        let (_ : unit P.t) = result |> P.then_ ~fulfilled in
        timeout (fail finish)

      let test_iter_ok finish =
        let promise = P.Result.return 5 in
        let (_ : unit P.t) =
          P.Result.iter
            ~ok:(fun o -> finish (fun () -> assert_equal o 5))
            ~error:(fun _ -> fail finish ())
            promise
        in
        timeout (fail finish)

      let test_iter_error finish =
        let promise = P.return (Result.Error "error") in
        let (_ : unit P.t) =
          P.Result.iter
            ~ok:(fun _ -> fail finish ())
            ~error:(fun e -> finish (fun () -> assert_equal e "error"))
            promise
        in
        timeout (fail finish)

      let suite =
        "Result"
        >::: [ "test_from_catch_fulfilled" >:~ test_from_catch_fulfilled
             ; "test_from_catch_rejected" >:~ test_from_catch_rejected
             ; "test_return" >:~ test_return
             ; "test_map_some" >:~ test_map_ok
             ; "test_map_error" >:~ test_map_error
             ; "test_bind_some" >:~ test_bind_ok
             ; "test_bind_error" >:~ test_bind_error
             ; "test_iter_ok" >:~ test_iter_ok
             ; "test_iter_error" >:~ test_iter_error
             ]
    end

    module SupplementalArray = struct
      let test_find_map finish =
        let promises = [| 1; 2; 3; 4; 5 |] in
        let f = function
          | 3 -> P.Option.return 3
          | _ -> P.return None
        in
        let fulfilled value =
          finish (fun () -> assert_equal value (Some 3));
          P.return ()
        in
        let result = P.Array.find_map f promises in
        let (_ : unit P.t) = result |> P.then_ ~fulfilled in
        timeout (fail finish)

      let test_filter_map finish =
        let promises = [| 1; 2; 3; 4; 5 |] in
        let f = function
          | (1 | 3 | 5) as x -> P.Option.return x
          | _                -> P.return None
        in
        let fulfilled value =
          finish (fun () -> assert_equal value [| 1; 3; 5 |]);
          P.return ()
        in
        let result = P.Array.filter_map f promises in
        let (_ : unit P.t) = result |> P.then_ ~fulfilled in
        timeout (fail finish)

      let suite =
        "Array"
        >::: [ "test_find_map" >:~ test_find_map
             ; "test_filter_map" >:~ test_filter_map
             ]
    end

    module SupplementalList = struct
      let test_find_map finish =
        let promises = [ 1; 2; 3; 4; 5 ] in
        let f = function
          | 3 -> P.Option.return 3
          | _ -> P.return None
        in
        let fulfilled value =
          finish (fun () -> assert_equal value (Some 3));
          P.return ()
        in
        let result = P.List.find_map f promises in
        let (_ : unit P.t) = result |> P.then_ ~fulfilled in
        timeout (fail finish)

      let test_filter_map finish =
        let promises = [ 1; 2; 3; 4; 5 ] in
        let f = function
          | (1 | 3 | 5) as x -> P.Option.return x
          | _                -> P.return None
        in
        let fulfilled value =
          finish (fun () -> assert_equal value [ 1; 3; 5 ]);
          P.return ()
        in
        let result = P.List.filter_map f promises in
        let (_ : unit P.t) = result |> P.then_ ~fulfilled in
        timeout (fail finish)

      let suite =
        "List"
        >::: [ "test_find_map" >:~ test_find_map
             ; "test_filter_map" >:~ test_filter_map
             ]
    end

    let suite =
      "Supplemental"
      >::: [ "test_return" >:~ test_return
           ; "test_map" >:~ test_map
           ; "test_bind" >:~ test_bind
           ; SupplementalSyntax.suite
           ; SupplementalOption.suite
           ; SupplementalResult.suite
           ; SupplementalArray.suite
           ; SupplementalList.suite
           ]
  end

  module Variance = struct
    type poly_variant =
      [ `A
      | `B
      ]

    type poly_variant_subtype = [ `A ]

    let test_variance_poly_variant finish =
      let promise_subtype : poly_variant_subtype P.t = P.return `A in
      let (_ : poly_variant P.t) = (promise_subtype :> poly_variant P.t) in
      pass finish ()

    type obj = < a : int >

    type obj_subtype = < a : int ; b : int >

    let test_variance_obj finish =
      let promise_subtype : obj_subtype P.t =
        P.return
          (object
             method a = 1

             method b = 2
          end)
      in
      let (_ : obj P.t) = (promise_subtype :> obj P.t) in
      pass finish ()

    let suite =
      "Variance"
      >::: [ "test_variance_poly_variant" >:~ test_variance_poly_variant
           ; "test_variance_obj" >:~ test_variance_obj
           ]
  end

  module Soundness = struct
    let test_make_soundness finish =
      let nested_promise =
        P.make (fun ~resolve ~reject:_ ->
            resolve (P.make (fun ~resolve ~reject:_ -> resolve 1)))
      in
      let fulfilled value =
        let fulfilled value =
          finish (fun () -> assert_equal value 1);
          P.resolve ()
        in
        let (_ : unit P.t) = value |> P.then_ ~fulfilled in
        P.resolve ()
      in
      let (_ : unit P.t) = nested_promise |> P.then_ ~fulfilled in
      timeout (fail finish)

    let test_resolve_soundness finish =
      let nested_promise = P.resolve (P.resolve 1) in
      let fulfilled value =
        let fulfilled value =
          finish (fun () -> assert_equal value 1);
          P.resolve ()
        in
        let (_ : unit P.t) = value |> P.then_ ~fulfilled in
        P.resolve ()
      in
      let (_ : unit P.t) = nested_promise |> P.then_ ~fulfilled in
      timeout (fail finish)

    let test_all_soundness finish =
      let promises = [| P.resolve (P.resolve 1); P.resolve (P.resolve 2) |] in
      let fulfilled value =
        let fulfilled value =
          finish (fun () -> assert_equal value [| 1; 2 |]);
          P.resolve ()
        in
        let result = P.all value in
        let (_ : unit P.t) = result |> P.then_ ~fulfilled in
        P.resolve ()
      in
      let result = P.all promises in
      let (_ : unit P.t) = result |> P.then_ ~fulfilled in
      timeout (fail finish)

    let test_all2_soundness finish =
      let promises = (P.resolve (P.resolve 1), P.resolve (P.resolve 2)) in
      let fulfilled value =
        let fulfilled value =
          finish (fun () -> assert_equal value (1, 2));
          P.resolve ()
        in
        let result = P.all2 value in
        let (_ : unit P.t) = result |> P.then_ ~fulfilled in
        P.resolve ()
      in
      let result = P.all2 promises in
      let (_ : unit P.t) = result |> P.then_ ~fulfilled in
      timeout (fail finish)

    let test_all3_soundness finish =
      let promises =
        ( P.resolve (P.resolve 1)
        , P.resolve (P.resolve 2)
        , P.resolve (P.resolve 3) )
      in
      let fulfilled value =
        let fulfilled value =
          finish (fun () -> assert_equal value (1, 2, 3));
          P.resolve ()
        in
        let result = P.all3 value in
        let (_ : unit P.t) = result |> P.then_ ~fulfilled in
        P.resolve ()
      in
      let result = P.all3 promises in
      let (_ : unit P.t) = result |> P.then_ ~fulfilled in
      timeout (fail finish)

    let test_race_soundness finish =
      let promises = [| P.resolve (P.resolve 5); timeout_promise finish |] in
      let fulfilled value =
        let fulfilled value =
          finish (fun () -> assert_equal value 5);
          P.resolve ()
        in
        let (_ : unit P.t) = value |> P.then_ ~fulfilled in
        P.resolve ()
      in
      let result = P.race promises in
      let (_ : unit P.t) = result |> P.then_ ~fulfilled in
      timeout (fail finish)

    let suite =
      "Soundness"
      >::: [ "test_make_soundness" >:~ test_make_soundness
           ; "test_resolve_soundness" >:~ test_resolve_soundness
           ; "test_all_soundness" >:~ test_all_soundness
           ; "test_all2_soundness" >:~ test_all2_soundness
           ; "test_all3_soundness" >:~ test_all3_soundness
           ; "test_race_soundness" >:~ test_race_soundness
           ]
  end

  let tests =
    [ CoreFunctions.suite
    ; InstanceMethods.suite
    ; StaticMethods.suite
    ; Supplemental.suite
    ; Variance.suite
    ; Soundness.suite
    ]
end

module LwtConversion = struct
  let test_of_promise_fulfilled finish =
    let js_promise = Promise.return 1 in
    let lwt_promise = Promise_lwt.of_promise js_promise in
    let fulfilled value = finish (fun () -> assert_equal value 1) in
    let rejected _ = fail finish () in
    Lwt.on_any lwt_promise fulfilled rejected;
    timeout (fail finish)

  let test_of_promise_rejected finish =
    let js_promise = Promise.reject @@ [%js.of: int] 2 in
    let lwt_promise = Promise_lwt.of_promise js_promise in
    let fulfilled _ = fail finish () in
    let rejected = function
      | Promise_lwt.Promise_error reason ->
        let reason = [%js.to: int] reason in
        finish (fun () -> assert_equal reason 2)
      | _ -> fail finish ()
    in
    Lwt.on_any lwt_promise fulfilled rejected;
    timeout (fail finish)

  let test_of_promise_throw finish =
    let js_promise =
      Promise.make (fun ~resolve:_ ~reject:_ -> ignore @@ eval "throw 3")
    in
    let lwt_promise = Promise_lwt.of_promise js_promise in
    let fulfilled _ = fail finish () in
    let rejected = function
      | Promise_lwt.Promise_error reason ->
        let reason = [%js.to: int] reason in
        finish (fun () -> assert_equal reason 3)
      | _ -> fail finish ()
    in
    Lwt.on_any lwt_promise fulfilled rejected;
    timeout (fail finish)

  let test_to_promise_fulfilled finish =
    let lwt_promise, resolver = Lwt.wait () in
    Lwt.wakeup_later resolver 1;
    let js_promise = Promise_lwt.to_promise lwt_promise in
    let fulfilled value =
      finish (fun () -> assert_equal value 1);
      Promise.return ()
    in
    let rejected _ =
      fail finish ();
      Promise.return ()
    in
    let (_ : unit Promise.t) = Promise.then_ js_promise ~fulfilled ~rejected in
    timeout (fail finish)

  let test_to_promise_rejected_exn finish =
    let exception E of int in
    let lwt_promise, resolver = Lwt.wait () in
    Lwt.wakeup_later_exn resolver (E 2);
    let js_promise = Promise_lwt.to_promise lwt_promise in
    let fulfilled _ =
      fail finish ();
      Promise.return ()
    in
    let rejected (reason : Promise.error) =
      let reason : exn = Obj.magic reason in
      finish (fun () -> assert_equal reason (E 2));
      Promise.return ()
    in
    let (_ : unit Promise.t) = Promise.then_ js_promise ~fulfilled ~rejected in
    timeout (fail finish)

  let test_to_promise_rejected_error finish =
    let js_promise_1 = Promise.reject @@ [%js.of: int] 3 in
    let lwt_promise = Promise_lwt.of_promise js_promise_1 in
    let js_promise_2 = Promise_lwt.to_promise lwt_promise in
    let fulfilled _ =
      fail finish ();
      Promise.return ()
    in
    let rejected (reason : Promise.error) =
      let reason = [%js.to: int] reason in
      finish (fun () -> assert_equal reason 3);
      Promise.return ()
    in
    let (_ : unit Promise.t) =
      Promise.then_ js_promise_2 ~fulfilled ~rejected
    in
    timeout (fail finish)

  let test_to_promise_raise finish =
    let exception E of int in
    let lwt_promise = Lwt.wrap (fun () -> raise (E 4)) in
    let js_promise = Promise_lwt.to_promise lwt_promise in
    let fulfilled _ =
      fail finish ();
      Promise.return ()
    in
    let rejected (reason : Promise.error) =
      let reason : exn = Obj.magic reason in
      finish (fun () -> assert_equal reason (E 4));
      Promise.return ()
    in
    let (_ : unit Promise.t) = Promise.then_ js_promise ~fulfilled ~rejected in
    timeout (fail finish)

  let suite =
    "LwtConversion"
    >::: [ "test_of_promise_fulfilled" >:~ test_of_promise_fulfilled
         ; "test_of_promise_rejected" >:~ test_of_promise_rejected
         ; "test_of_promise_throw" >:~ test_of_promise_throw
         ; "test_to_promise_fulfilled" >:~ test_to_promise_fulfilled
         ; "test_to_promise_rejected_exn" >:~ test_to_promise_rejected_exn
         ; "test_to_promise_rejected_error" >:~ test_to_promise_rejected_error
         ; "test_to_promise_raise" >:~ test_to_promise_raise
         ]
end

module Wrapped = Promise.Make (struct
  type +'a t = { js : Ojs.t }

  let t_of_js _ js = { js }

  let t_to_js _ { js } = js
end)

let suite =
  let module Simple = Tests (Promise) in
  let module Custom = Tests (Wrapped) in
  let module Double = Tests (Promise.Make (Promise)) in
  "Promise"
  >::: [ "Simple" >::: Simple.tests
       ; "Custom" >::: Custom.tests
       ; "Double" >::: Double.tests
       ; LwtConversion.suite
       ]

let () = Webtest_js.Runner.run suite
