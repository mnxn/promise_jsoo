type +'a t = 'a Lwt.t

exception Promise_error of Promise.error

let of_promise js_promise =
  let lwt_promise, resolver = Lwt.wait () in
  let fulfilled value =
    Lwt.wakeup_later resolver value;
    Promise.return ()
  in
  let rejected reason =
    Lwt.wakeup_later_exn resolver (Promise_error reason);
    Promise.return ()
  in
  let (_ : unit Promise.t) = Promise.then_ ~fulfilled ~rejected js_promise in
  lwt_promise

let to_promise lwt_promise =
  Promise.make @@ fun ~resolve ~reject ->
  let reject = function
    | Promise_error e -> reject e
    | e               -> reject (Obj.magic e : Promise.error)
  in
  Lwt.on_any lwt_promise resolve reject

let t_to_js (to_js : 'a -> Ojs.t) (lwt_promise : 'a t) : Ojs.t =
  Promise.t_to_js to_js (to_promise lwt_promise)

let t_of_js (of_js : Ojs.t -> 'a) (js_promise : Ojs.t) : 'a t =
  of_promise (Promise.t_of_js of_js js_promise)

type void = unit t

let void_to_js lwt_promise = Promise.void_to_js (to_promise lwt_promise)

let void_of_js js_promise = of_promise (Promise.void_of_js js_promise)
