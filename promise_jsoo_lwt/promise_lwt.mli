type +'a t = 'a Lwt.t

exception Promise_error of Promise.error
(** [Promise_error] is used to wrap JS promise errors
    in order to reject a Lwt promise *)

val of_promise : 'a Promise.t -> 'a t
(** [of_promise] converts from a JS promise to a Lwt promise

    If a JS promise is rejected with [e], the error value is wrapped
    and the Lwt promise is reject with [Promise_error e].
*)

val to_promise : 'a t -> 'a Promise.t
(** [to_promise] converts from a Lwt promise to a JS promise

    If a Lwt promise is rejected with [Promise_error e], the error value is
    unwrapped and the JS promise is rejected with [e].
*)

(** {1 Compatibility with gen_js_api} *)

val t_to_js : ('a -> Ojs.t) -> 'a t -> Ojs.t

val t_of_js : (Ojs.t -> 'a) -> Ojs.t -> 'a t

type void = unit t

val void_to_js : void -> Ojs.t

val void_of_js : Ojs.t -> void
