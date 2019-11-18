(**
   An implementation of a doubly linked list that can only insert elements at
   it's head and does not support removal of elements.

   The aim of this modules is to provide an abstract data type where given a
   position within the list, the user can seek forward or backwards an arbitary
   number of elements without traversing the whole list.
*)

(* Yeah, I know this is a terrible way to implement a doubly linked list, and
 * yeah, I know we probably could have used the standard implementation of
 * having a record with two pointer. But this is fun. *)

type 'a t
(** The abstract type of values representing a pointer to an element in a doubly
    linked list.
    The data structure is mutable -- inserting an element into the list changes
    the list. The pointers, however, always points to the same element or node
    regardless of how the list is changed.
*)

val empty : 'a t
(** [empty] is an empty doubly linked list *)

val is_empty : 'a t -> bool
(** [is_empty l] is whether [l] is an empty doubly linked list *)

val insert : 'a -> 'a t -> 'a t
(** [insert e l] is the pointer to the head of doubly linked list with [e]
    appended to [l].
    Requires: [l] needs to be a pointer to the head of the doubly linked list.
    Otherwise, raises NotHead.
*)

val to_singly_list : 'a t -> 'a list
(** [to_singly_list l] is the Ocaml List with the values of the doubly linked
    list of [l] starting from the pointer [l] *)

val get_value : 'a t -> 'a
(** [get_value l] is the value of the element at the pointer [l] *)

val prev : 'a t -> 'a t
(** [prev l] is the pointer to the previous element of [l]
    Raises: EndOfList if there is no previous element of [l].
    Raises; EmptyList if [l] is empty *)

val prev_opt : 'a t -> 'a t option
(** [prev_opt l] is [Some p] if p is the pointer to the previous element of [l],
    or [None] if the previous element of [l] does not exist or [l] is empty *)

val previ : 'a t -> int -> 'a t
(** [previ i l] is the pointer to [i] elemenets previous of [l].
    Raises: EndOfList if there is no ith previous element of [l].
    Raises; EmptyList if [l] is empty *)

val next : 'a t -> 'a t
(** [next l] is the pointer to the next element of [l]
    Raises: EndOfList if there is no next element of [l].
    Raises; EmptyList if [l] is empty *)


val next_opt : 'a t -> 'a t option
(** [next_opt l] is [Some p] if p is the pointer to the next element of [l],
    or [None] if the next element of [l] does not exist or [l] is empty *)

val nexti : 'a t -> int -> 'a t
(** [nexti i l] is the pointer to [i] elemenets next of [l].
    Raises: EndOfList if there is no ith next element of [l].
    Raises; EmptyList if [l] is empty *)
