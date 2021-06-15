module Promise : sig
  [@@@js.stop]

  type +'a t

  val t_of_js : (Ojs.t -> 'a) -> Ojs.t -> 'a t

  val t_to_js : ('a -> Ojs.t) -> 'a t -> Ojs.t

  [@@@js.start]

  [@@@js.implem
  type +'a t = Ojs.t

  let t_to_js to_js p = Ojs.call p "then" [| [%js.of: 'a -> Ojs.t] to_js |]

  let t_of_js of_js p = Ojs.call p "then" [| [%js.of: Ojs.t -> 'a] of_js |]]

  val new_ : (('a -> unit) -> ('e -> unit) -> unit) -> 'a t [@@js.create]

  val resolve : 'a -> 'a t [@@js.global]

  val reject : 'e -> 'a t [@@js.global]

  val catch : 'a t -> ('e -> 'a t) -> 'a t [@@js.call]

  val then_ :
    'a t -> fulfilled:('a -> 'b t) -> ?rejected:('error -> 'b t) -> unit -> 'b t
    [@@js.call]

  val finally : 'a t -> (unit -> unit) -> 'a t [@@js.call]

  val all : 'a t array -> 'a array t [@@js.global]

  val all2 : 'a t * 'b t -> ('a * 'b) t [@@js.global "all"]

  val all3 : 'a t * 'b t * 'c t -> ('a * 'b * 'c) t [@@js.global "all"]

  val race : 'a t array -> 'a t [@@js.global]
end
[@@js.scope "Promise"]

module IndirectPromise : sig
  val wrap : 'a -> 'a [@@js.global]

  val unwrap : 'a -> 'a [@@js.global]
end
[@@js.scope "IndirectPromise"]
