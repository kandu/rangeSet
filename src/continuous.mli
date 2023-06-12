(*
 * continuous.mli
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
end

module type S = sig
  type elt
  type t
  type point = Inc of elt | Exc of elt
  type range = { start : point; stop : point }

  val point_to_string : point -> string
  val range_to_string : range -> string
  val to_string : t -> string
  val empty : t
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val add : elt -> t -> t
  val singleton : elt -> t
  val of_point : point -> point -> t
  val of_range : range -> t
  val of_ranges : range list -> t
  val remove : elt -> t -> t
  val merge : range -> t -> t
  val unmerge : range -> t -> t
  val cover : range -> t -> t
  val union : t -> t -> t
  val diff : t -> t -> t
  val inter : t -> t -> t
end

module Make : functor (Ord : OrderedType) -> sig
  type elt = Ord.t
  type point = Inc of elt | Exc of elt
  type range = { start : point; stop : point }

  val point_to_string : point -> string
  val range_to_string : range -> string
  val extract_point : point -> elt
  val comp_point : point -> point
  val compare_point : point -> point -> int
  val compare_point_rev : point -> point -> int
  val compare_point_left : point -> point -> int
  val compare_point_right : point -> point -> int
  val point_include_left : point -> point -> bool
  val point_include_right : point -> point -> bool
  val is_closed : point -> point -> bool
  val _are_closed : point list -> bool

  module OrdR : sig
    type t = range

    val compare : range -> range -> int
  end

  module S : sig
    type elt = range
    type t = Set.Make(OrdR).t

    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val disjoint : t -> t -> bool
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val map : (elt -> elt) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val choose_opt : t -> elt option
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val of_list : elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val to_rev_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t
    val max_elt_opt : t -> elt option
    val min_elt_opt : t -> elt option
  end

  type t = S.t

  val to_string : S.t -> string
  val empty : S.t
  val is_empty : S.t -> bool
  val mem : elt -> S.t -> bool
  val add : elt -> S.t -> S.t
  val singleton : elt -> S.t
  val of_point : point -> point -> S.t
  val of_range : range -> S.t
  val remove : elt -> S.t -> S.t
  val merge : S.elt -> S.t -> S.t
  val unmerge : S.elt -> S.t -> S.t
  val cover : S.elt -> S.t -> S.t
  val union : S.t -> S.t -> S.t
  val diff : S.t -> S.t -> S.t
  val inter : S.t -> S.t -> S.t
  val of_ranges : range list -> S.t
end

