(*
 * discrete.mli
 * -----------
 * Copyright : (c) 2018 - 2023, ZAN DoYe <zandoye@gmail.com>
 * Licence   : MIT
 *
 * This file is a part of rangeSet.
 *)


module type OrderedType = sig
  type t

  val compare : t -> t -> int
  val to_string : t -> string
  val pred : t -> t
  val succ : t -> t
end

module type S = sig
  type elt
  type t
  type range = { start : elt; stop : elt }

  val range_to_string : range -> string
  val to_string : t -> string
  val empty : t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val singleton : elt -> t
  val of_point : elt -> elt -> t
  val of_range : range -> t
  val of_ranges : range list -> t
  val remove : elt -> t -> t
  val merge : range -> t -> t
  val unmerge : range -> t -> t
  val cover : range -> t -> t
  val union : t -> t -> t
  val diff : t -> t -> t
  val inter : t -> t -> t
  val min_elt : t -> elt
  val min_elt_opt : t -> elt option
  val max_elt : t -> elt
  val max_elt_opt : t -> elt option
  val iter_elt : (elt -> unit) -> t -> unit
  val map_elt : (elt -> elt) -> t -> t
  val elements : t -> elt list
end

module Make : functor (Ord : OrderedType) -> sig
  type elt = Ord.t
  type t
  type range = { start : elt; stop : elt }

  val range_to_string : range -> string
  val to_string : t -> string
  val empty : t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val singleton : elt -> t
  val of_point : elt -> elt -> t
  val of_range : range -> t
  val of_ranges : range list -> t
  val remove : elt -> t -> t
  val merge : range -> t -> t
  val unmerge : range -> t -> t
  val cover : range -> t -> t
  val union : t -> t -> t
  val diff : t -> t -> t
  val inter : t -> t -> t
  val min_elt : t -> elt
  val min_elt_opt : t -> elt option
  val max_elt : t -> elt
  val max_elt_opt : t -> elt option
  val iter_elt : (elt -> unit) -> t -> unit
  val map_elt : (elt -> elt) -> t -> t
  val elements : t -> elt list
end

