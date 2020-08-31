open Webtest.Suite

let pass finish () = finish Async.noop

let fail finish () = finish (fun () -> assert_true false)

let timeout f =
  let (_ : Js_of_ocaml.Dom_html.timeout_id_safe) =
    Js_of_ocaml.Dom_html.setTimeout f 1000.
  in
  ()

let timeout_promise finish =
  Promise.make (fun ~resolve:_ ~reject:_ -> timeout (fail finish))

module CoreFunctions = struct
  let test_make () =
    let promise = Promise.make (fun ~resolve:_ ~reject:_ -> ()) in
    assert_true Js_of_ocaml.Js.Optdef.(test (return promise));
    assert_true Js_of_ocaml.Js.Opt.(test (return promise))

  let test_resolve () =
    let promise = Promise.resolve () in
    assert_true Js_of_ocaml.Js.Optdef.(test (return promise));
    assert_true Js_of_ocaml.Js.Opt.(test (return promise))

  let test_reject () =
    let promise = Promise.resolve () in
    assert_true Js_of_ocaml.Js.Optdef.(test (return promise));
    assert_true Js_of_ocaml.Js.Opt.(test (return promise))

  let suite =
    "CoreFunctions"
    >::: [ "test_make" >:: test_make
         ; "test_resolve" >:: test_resolve
         ; "test_reject" >:: test_reject
         ]
end

module InstanceMethods = struct
  let test_then_fulfilled finish =
    let promise = Promise.resolve 1 in
    let fulfilled value =
      finish (fun () -> assert_equal value 1);
      Promise.resolve ()
    in
    let (_ : unit Promise.t) = Promise.then_ promise ~fulfilled in
    timeout (fail finish)

  let test_then_rejected finish =
    let promise = Promise.reject 2 in
    let fulfilled = Promise.resolve in
    let rejected (reason : Promise.error) =
      let reason : int = Obj.magic reason in
      finish (fun () -> assert_equal reason 2);
      Promise.resolve ()
    in
    let (_ : unit Promise.t) = Promise.then_ promise ~fulfilled ~rejected in
    timeout (fail finish)

  let test_catch finish =
    let promise = Promise.reject 3 in
    let rejected (reason : Promise.error) =
      let result : int = Obj.magic reason in
      finish (fun () -> assert_equal result 3);
      Promise.resolve ()
    in
    let (_ : unit Promise.t) = Promise.catch promise ~rejected in
    timeout (fail finish)

  let test_finally_resolved finish =
    let promise = Promise.resolve () in
    let (_ : unit Promise.t) = Promise.finally promise ~f:(pass finish) in
    timeout (fail finish)

  let test_finally_rejected finish =
    let promise = Promise.reject () in
    let rejected (_ : Promise.error) = Promise.return () in
    let (_ : unit Promise.t) =
      Promise.finally promise ~f:(pass finish) |> Promise.catch ~rejected
    in
    timeout (fail finish)

  let test_make_resolve finish =
    let promise = Promise.make (fun ~resolve ~reject:_ -> resolve 4) in
    let fulfilled value =
      finish (fun () -> assert_equal value 4);
      Promise.resolve ()
    in
    let (_ : unit Promise.t) = Promise.then_ promise ~fulfilled in
    timeout (fail finish)

  let test_make_reject finish =
    let promise = Promise.make (fun ~resolve:_ ~reject -> reject 5) in
    let rejected (reason : Promise.error) =
      let reason : int = Obj.magic reason in
      finish (fun () -> assert_equal reason 5);
      Promise.resolve ()
    in
    let (_ : unit Promise.t) = Promise.catch promise ~rejected in
    timeout (fail finish)

  let suite =
    "InstanceMethods"
    >::: [ "test_then_fulfilled" >:~ test_then_fulfilled
         ; "test_then_rejected" >:~ test_then_rejected
         ; "test_catch" >:~ test_catch
         ; "test_finally_resolved" >:~ test_finally_resolved
         ; "test_finally_rejected" >:~ test_finally_rejected
         ; "test_make_resolve" >:~ test_make_resolve
         ; "test_make_reject" >:~ test_make_reject
         ]
end

module StaticMethods = struct
  let test_all finish =
    let promises =
      [| Promise.resolve 1; Promise.resolve 2; Promise.resolve 3 |]
    in
    let fulfilled value =
      finish (fun () -> assert_equal value [| 1; 2; 3 |]);
      Promise.resolve ()
    in
    let result = Promise.all promises in
    let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
    timeout (fail finish)

  let test_all_list finish =
    let promises =
      [ Promise.resolve 1; Promise.resolve 2; Promise.resolve 3 ]
    in
    let fulfilled value =
      finish (fun () -> assert_equal value [ 1; 2; 3 ]);
      Promise.resolve ()
    in
    let result = Promise.all_list promises in
    let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
    timeout (fail finish)

  let test_all2 finish =
    let promises = (Promise.resolve 1, Promise.resolve "2") in
    let fulfilled value =
      finish (fun () -> assert_equal value (1, "2"));
      Promise.resolve ()
    in
    let result = Promise.all2 promises in
    let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
    timeout (fail finish)

  let test_all3 finish =
    let promises =
      (Promise.resolve true, Promise.resolve 2, Promise.resolve "3")
    in
    let fulfilled value =
      finish (fun () -> assert_equal value (true, 2, "3"));
      Promise.resolve ()
    in
    let result = Promise.all3 promises in
    let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
    timeout (fail finish)

  let test_race finish =
    let promises = [| Promise.resolve 1; timeout_promise finish |] in
    let fulfilled value =
      finish (fun () -> assert_equal value 1);
      Promise.resolve ()
    in
    let result = Promise.race promises in
    let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
    timeout (fail finish)

  let test_race_list finish =
    let promises = [ Promise.resolve 1; timeout_promise finish ] in
    let fulfilled value =
      finish (fun () -> assert_equal value 1);
      Promise.resolve ()
    in
    let result = Promise.race_list promises in
    let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
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
    let promise = Promise.return 5 in
    let fulfilled value =
      finish (fun () -> assert_equal value 5);
      Promise.resolve ()
    in
    let (_ : unit Promise.t) = promise |> Promise.then_ ~fulfilled in
    timeout (fail finish)

  let test_map finish =
    let promise = Promise.return 2 in
    let fulfilled value =
      finish (fun () -> assert_equal value 6);
      Promise.return ()
    in
    let result = Promise.map (fun x -> x * 3) promise in
    let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
    timeout (fail finish)

  let test_bind finish =
    let promise = Promise.return 12 in
    let fulfilled value =
      finish (fun () -> assert_equal value 3);
      Promise.return ()
    in
    let result = Promise.bind (fun x -> Promise.return (x / 4)) promise in
    let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
    timeout (fail finish)

  module SupplementalSyntax = struct
    let test_map_op finish =
      let promise = Promise.return 2 in
      let fulfilled value =
        finish (fun () -> assert_equal value 6);
        Promise.return ()
      in
      let result =
        let open Promise.Syntax in
        promise >>| fun x -> x * 3
      in
      let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
      timeout (fail finish)

    let test_bind_op finish =
      let promise = Promise.return 12 in
      let fulfilled value =
        finish (fun () -> assert_equal value 3);
        Promise.return ()
      in
      let result =
        let open Promise.Syntax in
        promise >>= fun x -> Promise.return (x / 4)
      in
      let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
      timeout (fail finish)

    let test_map_let_op finish =
      let promise = Promise.return 2 in
      let fulfilled value =
        finish (fun () -> assert_equal value 6);
        Promise.return ()
      in
      let result =
        let open Promise.Syntax in
        let+ x = promise in
        x * 3
      in
      let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
      timeout (fail finish)

    let test_bind_let_op finish =
      let promise = Promise.return 12 in
      let fulfilled value =
        finish (fun () -> assert_equal value 3);
        Promise.return ()
      in
      let result =
        let open Promise.Syntax in
        let* x = promise in
        Promise.return (x / 4)
      in
      let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
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
      let promise = Promise.Option.return 5 in
      let fulfilled value =
        finish (fun () -> assert_equal value (Some 5));
        Promise.return ()
      in
      let (_ : unit Promise.t) = promise |> Promise.then_ ~fulfilled in
      timeout (fail finish)

    let test_map_some finish =
      let promise = Promise.Option.return 2 in
      let fulfilled value =
        finish (fun () -> assert_equal value (Some 6));
        Promise.return ()
      in
      let result = Promise.Option.map (fun x -> x * 3) promise in
      let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
      timeout (fail finish)

    let test_map_none finish =
      let promise = Promise.return None in
      let fulfilled value =
        finish (fun () -> assert_equal value None);
        Promise.return ()
      in
      let result = Promise.Option.map (fun x -> x * 3) promise in
      let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
      timeout (fail finish)

    let test_bind_some finish =
      let promise = Promise.Option.return 12 in
      let fulfilled value =
        finish (fun () -> assert_equal value (Some 3));
        Promise.return ()
      in
      let result =
        Promise.Option.bind (fun x -> Promise.Option.return (x / 4)) promise
      in
      let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
      timeout (fail finish)

    let test_bind_none finish =
      let promise = Promise.return None in
      let fulfilled value =
        finish (fun () -> assert_equal value None);
        Promise.return ()
      in
      let result =
        Promise.Option.bind (fun x -> Promise.Option.return (x / 4)) promise
      in
      let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
      timeout (fail finish)

    let test_iter_some finish =
      let promise = Promise.Option.return 5 in
      let fulfilled () =
        fail finish ();
        Promise.return ()
      in
      let f value = finish (fun () -> assert_equal value 5) in
      let result = Promise.Option.iter f promise in
      let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
      timeout (fail finish)

    let test_iter_none finish =
      let promise = Promise.return None in
      let fulfilled () =
        pass finish ();
        Promise.resolve ()
      in
      let result = Promise.Option.iter (fail finish) promise in
      let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
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
      let promise = Promise.resolve 1 in
      let fulfilled value =
        finish (fun () -> assert_equal value (Ok 1));
        Promise.return ()
      in
      let result = Promise.Result.from_catch promise in
      let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
      timeout (fail finish)

    let test_from_catch_rejected finish =
      let promise = Promise.reject "error" in
      let fulfilled (value : ('a, Promise.error) Result.t) =
        let value = Obj.magic value in
        finish (fun () -> assert_equal value (Result.Error "error"));
        Promise.return ()
      in
      let result = Promise.Result.from_catch promise in
      let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
      timeout (fail finish)

    let test_return finish =
      let promise = Promise.Result.return 5 in
      let fulfilled value =
        finish (fun () -> assert_equal value (Ok 5));
        Promise.return ()
      in
      let (_ : unit Promise.t) = promise |> Promise.then_ ~fulfilled in
      timeout (fail finish)

    let test_map_ok finish =
      let promise = Promise.Result.return 2 in
      let fulfilled value =
        finish (fun () -> assert_equal value (Ok 6));
        Promise.return ()
      in
      let result = Promise.Result.map (fun x -> x * 3) promise in
      let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
      timeout (fail finish)

    let test_map_error finish =
      let promise = Promise.return (Result.Error "error") in
      let fulfilled value =
        finish (fun () -> assert_equal value (Result.Error "error"));
        Promise.return ()
      in
      let result = Promise.Result.map (fun x -> x * 3) promise in
      let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
      timeout (fail finish)

    let test_bind_ok finish =
      let promise = Promise.Result.return 12 in
      let fulfilled value =
        finish (fun () -> assert_equal value (Ok 3));
        Promise.return ()
      in
      let result =
        Promise.Result.bind (fun x -> Promise.Result.return (x / 4)) promise
      in
      let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
      timeout (fail finish)

    let test_bind_error finish =
      let promise = Promise.return (Result.Error "error") in
      let fulfilled value =
        finish (fun () -> assert_equal value (Result.error "error"));
        Promise.return ()
      in
      let result =
        Promise.Result.bind (fun x -> Promise.Result.return (x / 4)) promise
      in
      let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
      timeout (fail finish)

    let test_iter_ok finish =
      let promise = Promise.Result.return 5 in
      let (_ : unit Promise.t) =
        Promise.Result.iter
          ~ok:(fun o -> finish (fun () -> assert_equal o 5))
          ~error:(fun _ -> fail finish ())
          promise
      in
      timeout (fail finish)

    let test_iter_error finish =
      let promise = Promise.return (Result.Error "error") in
      let (_ : unit Promise.t) =
        Promise.Result.iter
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
        | 3 -> Promise.Option.return 3
        | _ -> Promise.return None
      in
      let fulfilled value =
        finish (fun () -> assert_equal value (Some 3));
        Promise.return ()
      in
      let result = Promise.Array.find_map f promises in
      let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
      timeout (fail finish)

    let test_filter_map finish =
      let promises = [| 1; 2; 3; 4; 5 |] in
      let f = function
        | (1 | 3 | 5) as x -> Promise.Option.return x
        | _                -> Promise.return None
      in
      let fulfilled value =
        finish (fun () -> assert_equal value [| 1; 3; 5 |]);
        Promise.return ()
      in
      let result = Promise.Array.filter_map f promises in
      let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
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
        | 3 -> Promise.Option.return 3
        | _ -> Promise.return None
      in
      let fulfilled value =
        finish (fun () -> assert_equal value (Some 3));
        Promise.return ()
      in
      let result = Promise.List.find_map f promises in
      let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
      timeout (fail finish)

    let test_filter_map finish =
      let promises = [ 1; 2; 3; 4; 5 ] in
      let f = function
        | (1 | 3 | 5) as x -> Promise.Option.return x
        | _                -> Promise.return None
      in
      let fulfilled value =
        finish (fun () -> assert_equal value [ 1; 3; 5 ]);
        Promise.return ()
      in
      let result = Promise.List.filter_map f promises in
      let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
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
    let promise_subtype : poly_variant_subtype Promise.t = Promise.return `A in
    let (_ : poly_variant Promise.t) =
      (promise_subtype :> poly_variant Promise.t)
    in
    pass finish ()

  type obj = < a : int >

  type obj_subtype = < a : int ; b : int >

  let test_variance_obj finish =
    let promise_subtype : obj_subtype Promise.t =
      Promise.return
        (object
           method a = 1

           method b = 2
        end)
    in
    let (_ : obj Promise.t) = (promise_subtype :> obj Promise.t) in
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
      Promise.make (fun ~resolve ~reject:_ ->
          resolve (Promise.make (fun ~resolve ~reject:_ -> resolve 1)))
    in
    let fulfilled value =
      let fulfilled value =
        finish (fun () -> assert_equal value 1);
        Promise.resolve ()
      in
      let (_ : unit Promise.t) = value |> Promise.then_ ~fulfilled in
      Promise.resolve ()
    in
    let (_ : unit Promise.t) = nested_promise |> Promise.then_ ~fulfilled in
    timeout (fail finish)

  let test_resolve_soundness finish =
    let nested_promise = Promise.resolve (Promise.resolve 1) in
    let fulfilled value =
      let fulfilled value =
        finish (fun () -> assert_equal value 1);
        Promise.resolve ()
      in
      let (_ : unit Promise.t) = value |> Promise.then_ ~fulfilled in
      Promise.resolve ()
    in
    let (_ : unit Promise.t) = nested_promise |> Promise.then_ ~fulfilled in
    timeout (fail finish)

  let test_all_soundness finish =
    let promises =
      [| Promise.resolve (Promise.resolve 1)
       ; Promise.resolve (Promise.resolve 2)
      |]
    in
    let fulfilled value =
      let fulfilled value =
        finish (fun () -> assert_equal value [| 1; 2 |]);
        Promise.resolve ()
      in
      let result = Promise.all value in
      let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
      Promise.resolve ()
    in
    let result = Promise.all promises in
    let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
    timeout (fail finish)

  let test_all2_soundness finish =
    let promises =
      (Promise.resolve (Promise.resolve 1), Promise.resolve (Promise.resolve 2))
    in
    let fulfilled value =
      let fulfilled value =
        finish (fun () -> assert_equal value (1, 2));
        Promise.resolve ()
      in
      let result = Promise.all2 value in
      let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
      Promise.resolve ()
    in
    let result = Promise.all2 promises in
    let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
    timeout (fail finish)

  let test_all3_soundness finish =
    let promises =
      ( Promise.resolve (Promise.resolve 1)
      , Promise.resolve (Promise.resolve 2)
      , Promise.resolve (Promise.resolve 3) )
    in
    let fulfilled value =
      let fulfilled value =
        finish (fun () -> assert_equal value (1, 2, 3));
        Promise.resolve ()
      in
      let result = Promise.all3 value in
      let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
      Promise.resolve ()
    in
    let result = Promise.all3 promises in
    let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
    timeout (fail finish)

  let test_race_soundness finish =
    let promises =
      [| Promise.resolve (Promise.resolve 5); timeout_promise finish |]
    in
    let fulfilled value =
      let fulfilled value =
        finish (fun () -> assert_equal value 5);
        Promise.resolve ()
      in
      let (_ : unit Promise.t) = value |> Promise.then_ ~fulfilled in
      Promise.resolve ()
    in
    let result = Promise.race promises in
    let (_ : unit Promise.t) = result |> Promise.then_ ~fulfilled in
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

let suite =
  "Promise"
  >::: [ CoreFunctions.suite
       ; InstanceMethods.suite
       ; StaticMethods.suite
       ; Supplemental.suite
       ; Variance.suite
       ; Soundness.suite
       ]

let () = Webtest_js.Runner.run suite
