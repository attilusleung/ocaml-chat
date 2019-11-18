(* TODO: Replace all failwith with actual exceptions *)
open Log

exception EndOfList

exception EmptyList

exception NotHead

type 'a entry = {value: 'a; mutable backptr: 'a entry list option}
(** Represents an entry in the doubly linked list. It contains a value of the
 * element, as well as a backpointer to the next element in the list *)

type 'a t = 'a entry list
(** The abstract type of the doubly linked list. It is just a list of entries.
 * *)

(* AF: the list
 * [{value=v1; backptr=b1}; ...; {value=vn; backptr=bn}]
      represents the list [v1; ... vn]

      RI: For all entry ei in the list [e1; ...; en], e_i.backptr points to the
      Some list [e(i-1); ei; ...; en]. e1.backptr should point to None.
    *)

let empty = []

(** [rep_ok t] is [t] if [t] satisfies the representation invariant. It fails
    with an assertion error otherwise *)
let rep_ok t =
  match t with
  (* RI satisfied if empty list *)
  | [] ->
    t
  (* Check first element backptr is None *)
  | h :: ta ->
    if h.backptr <> None then assert false
    else
      (* [pointer_ok t] is whether t has valid pointers *)
      let rec pointer_ok t =
        match t with
        | [] ->
          true
        | _ :: ta as l -> (
            (* Nested match statements are necessary to preserve physical equality
             * of variables *)
            match ta with
            | [] ->
              true
            (* Checks if backpointer of the second element in the list points to
             * the head of the list *)
            | {backptr= Some b} :: _ ->
              if b == l then pointer_ok ta else false
            (* Only the first element of the list should have a None value for
             * backptr *)
            | {backptr= None} :: _ ->
              false )
      in
      assert (pointer_ok t) ;
      t

let is_empty t = t = empty

let rec insert value t =
  let l = {value; backptr= None} :: t in
  ( match t with
    | [] ->
      ()
    | ({backptr= None} as h) :: t ->
      h.backptr <- Some l
    | {backptr= Some p} :: _ ->
      raise NotHead ) ;
  l

let to_singly_list t = List.map (fun a -> a.value) t

let get_value t = match t with [] -> raise EmptyList | h :: t -> h.value

let next t =
  match t with
  | [] ->
    raise EmptyList
  | {backptr= None} :: _ ->
    raise EndOfList
  | {backptr= Some p} :: _ ->
    p

let next_opt t = try Some (next t) with EndOfList -> None | EmptyList -> None

let rec nexti t i =
  if i = 0 then t
  else
    match t with
    | {backptr= Some b} :: _ ->
      nexti b (i - 1)
    | {backptr= None} :: _ ->
      raise EndOfList
    | [] ->
      raise EmptyList

let prev t =
  match t with [] -> raise EmptyList | [h] -> raise EndOfList | h :: t -> t

let prev_opt t = try Some (prev t) with EndOfList -> None | EmptyList -> None

let rec previ t i =
  if i = 0 then t
  else
    match t with
    | [] ->
      raise EmptyList
    | [h] ->
      raise EndOfList
    | h :: t ->
      previ t (i - 1)
