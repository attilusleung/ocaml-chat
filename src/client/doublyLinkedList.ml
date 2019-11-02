(* TODO: Replace all failwith with actual exceptions *)

type 'a entry =
  { value: 'a; mutable backptr: 'a entry list option }

type 'a t = 'a entry list
(* AF: the list [{value=v1; backptr=b1}; ...; {value=vn; backptr=bn}] represents
                  * the list [v1; ... vn]
                  *
                  * RI: For all entry ei in the list [e1; ...; en], e_i.backptr points to the
                  * Some list [e(i-1); ei; ...; en]. e1.backptr should point to None.
                 *)

let empty = []

let rep_ok t =
  match t with
  (* RI satisfied if empty list *)
  | [] -> true
  (* Check first element backptr is None *)
  | h::ta -> if h.backptr <> None then false else
      (* [pointer_ok t] is whether t has valid pointers *)
      let rec pointer_ok t =
        match t with
        | [] -> true
        | _::ta as l ->
          (* Nested match statements are necessary to preserve physical equality
           * of variables *)
          match ta with
          | [] -> true
          (* Checks if backpointer of the second element in the list points to
           * the head of the list *)
          |{backptr= Some b}::_ -> if b == l then
              pointer_ok ta else false
          (* Only the first element of the list should have a None value for
           * backptr *)
          |{backptr= None}::_ -> false
      in
      pointer_ok t

let insert value t =
  let l = {value= value; backptr= None}::t in
  (match t with
   | [] -> ()
   | h::t -> (h.backptr <- Some l));
  l

let to_singly_list t =
  List.map (fun a -> a.value) t

let get_value t =
  match t with
  | [] -> failwith "empty"
  | h::t -> h.value

let prev t =
  match t with
  | [] -> failwith "list empty"
  | {backptr= None}::_ -> failwith "already head"
  | {backptr= Some p}::_ -> p

let rec previ t i =
  if i = 0 then t
  else match t with
    | {backptr= Some b}::_ -> previ b (i-1)
    | {backptr= None}::_ -> failwith "end of list"
    | [] -> failwith "RI violated"

let next t =
  match t with
  | [] -> failwith "list empty"
  | h::t -> t

let rec nexti t i =
  if i = 0 then t
  else match t with
    | [] -> failwith "end of list"
    | h::t -> nexti t (i-1)
