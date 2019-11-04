(**
   An implementation of a doubly linked list that can only insert elements at
   it's head and does not support removal of elements.

   The aim of this modules is to provide an abstract data type where given a
   position within the list, the user can seek forward or backwards an arbitary
   number of elements without traversing the whole list.
*)

(** The abstract type of values representing the doubly linked list *)
type 'a t
(* type 'a entry = *)
(*   { value: 'a; mutable backptr: 'a entry list option } *)
(*  *)
(* type 'a t = 'a entry list *)

val empty: 'a t

val is_empty: 'a t -> bool

val insert: 'a -> 'a t -> 'a t

val to_singly_list: 'a t -> 'a list

val get_value: 'a t -> 'a

val prev: 'a t -> 'a t

val prev_opt: 'a t -> 'a t option

val previ: 'a t -> int -> 'a t

val next: 'a t -> 'a t

val next_opt: 'a t -> 'a t option

val nexti: 'a t -> int -> 'a t
