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
  (** The type of the set elements. *)

  val compare : t -> t -> int
  (** A total ordering function over the set elements.
      This is a two-argument function [f] such that
      [f e1 e2] is zero if the elements [e1] and [e2] are equal,
      [f e1 e2] is strictly negative if [e1] is smaller than [e2],
      and [f e1 e2] is strictly positive if [e1] is greater than [e2].
      Example: a suitable ordering function is the generic structural
      comparison function {!Pervasives.compare}. *)


  val to_string : t -> string
  (** [to_string t] is the written representation of [t]. *)

  val pred : t -> t
  (** [pred t] is the predecessor value of t. *)

  val succ : t -> t
  (** [succ t] is the successor value of t. *)
end
(** Input signature of the functor {!RangeSet.Discrete.Make}. *)


module type S = sig
  type elt
  (** The type of the set elements. *)

  type t
  (** The type of the sets. *)

  type range = { start : elt; stop : elt }
  (** The type of the range.
      A range consists of [start] and [end] endpoint.
      *)

  val range_to_string : range -> string
  (** [range_to_string range] is the written representation of [range]. *)

  val to_string : t -> string
  (** [to_string set] is the written representation of [set]. *)

  val empty : t
  (** The empty set. *)

  val is_empty : t -> bool
  (** [is_empty t] tests whether [t] is empty or not. *)

  val mem : elt -> t -> bool
  (** [mem x s] tests whether [x] belongs to the set [s]. *)

  val add : elt -> t -> t
  (** [add x s] returns a set containing all elements of [s],
    plus [x]. If [x] was already in [s], [s] is returned unchanged. *)

  val singleton : elt -> t
  (** [singleton x] returns the one-element set containing only [x]. *)

  val of_point : elt -> elt -> t
  (** [of_point p1 p2] returns the set containing a range from p1 to p2.
    *)

  val of_range : range -> t
  (** [of_range r] returns the set containing only the [range]. *)

  val of_ranges : range list -> t
  (** [of_ranges rs] returns the set containing all the ranges in [rs]. *)

  val remove : elt -> t -> t
  (** [remove x s] returns a set containing all elements of s, except x. *)

  val merge : range -> t -> t
  (** [merge r s] returns a set containing all elements of s
    , plus range [r]. *)

  val unmerge : range -> t -> t
  (** [unmerge r s] returns a set containing all elements of s, except those in range [r]. *)

  val cover : range -> t -> t
  (** [cover r s] returns a set containing elements both belongs to range [r] and set [s]. *)

  val union : t -> t -> t
  (** Set union. *)

  val diff : t -> t -> t
  (** Set difference: [diff s1 s2] contains the elements of [s1]
      that are not in [s2]. *)

  val inter : t -> t -> t
  (** Set intersection. *)

  val min_elt : t -> elt
  (** Return the smallest element of the given set
      (with respect to the [Ord.compare] ordering), or raise
      [Not_found] if the set is empty. *)

  val min_elt_opt : t -> elt option
  (** Return the smallest element of the given set
      (with respect to the [Ord.compare] ordering), or [None]
      if the set is empty. *)

  val max_elt : t -> elt
  (** Return the largest element of the given set
    (with respect to the [Ord.compare] ordering), or raise
    [Not_found] if the set is empty. *)

  val max_elt_opt : t -> elt option
  (** Return the largest element of the given set
    (with respect to the [Ord.compare] ordering), or [None]
    if the set is empty. *)

  val iter_elt : (elt -> unit) -> t -> unit
  (** [iter_elt f s] applies [f] in turn to all elements of [s].
    The elements of [s] are presented to [f] in increasing order
    with respect to the ordering over the type of the elements. *)

  val map_elt : (elt -> elt) -> t -> t
  (** [map_elt f s] is the set whose elements are [f a0],[f a1]... [f
      aN], where [a0],[a1]...[aN] are the elements of [s].

      The elements are passed to [f] in increasing order
      with respect to the ordering over the type of the elements. *)

  val elements : t -> elt list
  (** Return the list of all elements of the given set.
    The returned list is sorted in increasing order
    with respect to the ordering [Ord.compare],
    where Ord is the argument given to [Discrete.Make]. *)
end
(** Output signature of the functor {!RangeSet.Discrete.Make}. *)


module Make : functor (Ord : OrderedType) -> S with type elt:= Ord.t
(** Functor building an implementation of the RangeSet.Discrete structure
   given a totally ordered type. *)
