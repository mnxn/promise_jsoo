include Promise_intf

module Make (T : P) : S with type 'a t = 'a T.t = struct
  module P = Internal.Promise (T)
  module IP = Internal.IndirectPromise

  type +'a t = 'a P.t

  let t_of_js = P.t_of_js

  let t_to_js = P.t_to_js

  type error = Ojs.t [@@js]

  let make (f : resolve:('a -> unit) -> reject:(error -> unit) -> unit) : 'a t =
    let f_safe resolve reject =
      let resolve_safe value = resolve (IP.wrap value) in
      f ~resolve:resolve_safe ~reject
    in
    P.new_ f_safe

  let resolve (value : 'a) : 'a t = P.resolve (IP.wrap value)

  let reject (reason : error) : 'a t = P.reject reason

  let catch ~rejected promise = P.catch promise rejected

  let then_ ~fulfilled ?rejected promise =
    let fulfilled_safe value = fulfilled (IP.unwrap value) in
    P.then_ promise ~fulfilled:fulfilled_safe ?rejected ()

  let finally ~f promise = P.finally promise f

  let all promises =
    P.all promises
    |> then_ ~fulfilled:(fun arr -> resolve (Array.map IP.unwrap arr))

  let all2 promises =
    P.all2 promises
    |> then_ ~fulfilled:(fun (v1, v2) -> resolve (IP.unwrap v1, IP.unwrap v2))

  let all3 promises =
    P.all3 promises
    |> then_ ~fulfilled:(fun (v1, v2, v3) ->
           resolve (IP.unwrap v1, IP.unwrap v2, IP.unwrap v3))

  let all_list promises =
    all (Array.of_list promises)
    |> then_ ~fulfilled:(fun value -> resolve (Array.to_list value))

  let race = P.race

  let race_list (promises : 'a t list) : 'a t = race (Array.of_list promises)

  let return = resolve

  let map f promise = then_ ~fulfilled:(fun value -> return (f value)) promise

  let bind f promise = then_ ~fulfilled:f promise

  module Syntax = struct
    let ( >>| ) x f = map f x

    let ( >>= ) x f = bind f x

    let ( let+ ) x f = map f x

    let ( let* ) x f = bind f x
  end

  module Array = struct
    let find_map (f : 'a -> 'b option t) (arr : 'a array) : 'b option t =
      let len = Array.length arr in
      let rec loop i =
        if i = len then
          return None
        else
          let open Syntax in
          let* res = f arr.(i) in
          match res with
          | None        -> loop (succ i)
          | Some _ as x -> return x
      in
      loop 0

    let filter_map (f : 'a -> 'b option t) (arr : 'a array) : 'b array t =
      let open Syntax in
      let+ arr = all (Array.map f arr) in
      let filter_map = List.filter_map (fun x -> x) in
      Array.of_list (filter_map (Array.to_list arr))
  end

  module List = struct
    let rec find_map (f : 'a -> 'b option t) = function
      | []      -> return None
      | x :: xs -> (
        let open Syntax in
        let* res = f x in
        match res with
        | None        -> find_map f xs
        | Some _ as x -> return x)

    let filter_map (f : 'a -> 'b option t) (xs : 'a list) : 'b list t =
      let open Syntax in
      let+ xs = all_list (List.map f xs) in
      List.filter_map (fun x -> x) xs
  end

  module Option = struct
    let iter f =
      bind @@ function
      | Some x -> return (f x : unit)
      | None   -> return ()

    let map f =
      bind @@ function
      | Some x -> return (Some (f x))
      | None   -> return None

    let bind f =
      bind @@ function
      | Some x -> f x
      | None   -> return None

    let return x = return (Some x)

    module Syntax = struct
      let ( >>| ) x f = map f x

      let ( >>= ) x f = bind f x

      let ( let+ ) x f = map f x

      let ( let* ) x f = bind f x
    end
  end

  module Result = struct
    let from_catch promise =
      let fulfilled value = return (Ok value) in
      let rejected reason = return (Error reason) in
      promise |> then_ ~fulfilled ~rejected

    let iter ?(ok = ignore) ?(error = ignore) =
      bind @@ function
      | Ok o    -> return (ok o)
      | Error e -> return (error e)

    let map f =
      bind @@ function
      | Ok o    -> return (Ok (f o))
      | Error e -> return (Error e)

    let bind f =
      bind @@ function
      | Ok o    -> f o
      | Error e -> return (Error e)

    let return x = return (Ok x)

    module Syntax = struct
      let ( >>| ) x f = map f x

      let ( >>= ) x f = bind f x

      let ( let+ ) x f = map f x

      let ( let* ) x f = bind f x
    end
  end
end

include Make (struct
  type +'a t = Ojs.t

  let t_of_js _ = Ojs.t_of_js

  let t_to_js _ = Ojs.t_to_js
end)
