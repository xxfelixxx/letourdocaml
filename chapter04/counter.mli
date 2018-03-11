(* counter.mli is the Interface aka Signature aka Module Type for counter.ml *)

open Core

(* starting comments with ** means ocamldoc will find them *)
(** A collection of string frequency types *)
type t

(** The empty set of frequency counts *)
val empty : t

(** Bump the frequency count for the given string *)
val touch : t -> string -> t

(** Converts the set of frequency counts to an association list.
    A string shows up at most once, and the counts are >= 1. *)
val to_list : t -> (string * int) list
