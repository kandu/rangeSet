(*
 * utils.ml
 * -----------
 * Copyright : (c) 2018 - 2023, ZAN DoYe <zandoye@gmail.com>
 * Licence   : MIT
 *
 * This file is a part of rangeSet.
 *)


(* compatible with ocaml 4.02 *)
let option_get opt=
  match opt with
  | Some v-> v
  | None-> invalid_arg "option is None"
