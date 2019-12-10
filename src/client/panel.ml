open Unix
open ANSITerminal
open Key
open Network
open Lwt
open Log
open Printexc
open Parser
open Client

exception PanelWidthTooLarge

exception PanelHeightTooLarge

module Panel = struct
  type t = {x: int; y: int; width: int; height: int}

  (* h: horizontal
   * v: vertical
   * u: upper
   * l: lower
   * l: left
   * r: right
   * b: bold *)
  (* Where is unicode escapes in the Ocaml Documentation? *)

  (** [hline] is the unicode character of a horizontal line *)
  let hline = "\u{2500}"

  (** [hbline] is the unicode character of a horizontal bold line *)
  let hbline = "\u{2501}"

  (** [vline] is the unicode character of a vertical line *)
  let vline = "\u{2502}"

  (** [vbline] is the unicode character of a vertical bold line *)
  let vbline = "\u{2503}"

  (** [ulcorner] is the unicode character of a upper left corner *)
  let ulcorner = "\u{250C}"

  (** [ulbcorner] is the unicode character of a upper left bold corner *)
  let ulbcorner = "\u{250F}"

  (** [urcorner] is the unicode character of a upper right corner *)
  let urcorner = "\u{2510}"

  (** [urbcorner] is the unicode character of a upper right bold corner *)
  let urbcorner = "\u{2513}"

  (** [llcorner] is the unicode character of a lower left corner *)
  let llcorner = "\u{2514}"

  (** [llbcorner] is the unicode character of a lower left bold corner *)
  let llbcorner = "\u{2517}"

  (** [lrcorner] is the unicode character of a lower right corner *)
  let lrcorner = "\u{2518}"

  (** [lrbcorner] is the unicode character of a lower right bold corner *)
  let lrbcorner = "\u{251B}"

  let make x y width height =
    assert (x >= 0) ;
    assert (y >= 0) ;
    assert (width > 1) ;
    assert (height > 1) ;
    {x; y; width; height}

  let draw_border t buffer strong =
    if t.y + t.height > Array.length buffer.(0) then raise PanelHeightTooLarge ;
    if t.x + t.width > Array.length buffer then raise PanelWidthTooLarge ;
    buffer.(t.x).(t.y) <- (if strong then ulbcorner else ulcorner) ;
    buffer.(t.x).(t.y + t.height - 1) <-
      (if strong then llbcorner else llcorner) ;
    buffer.(t.x + t.width - 1).(t.y) <- (if strong then urbcorner else urcorner) ;
    buffer.(t.x + t.width - 1).(t.y + t.height - 1) <-
      (if strong then lrbcorner else lrcorner) ;
    for i = t.x + 1 to t.x + t.width - 2 do
      buffer.(i).(t.y) <- (if strong then hbline else hline) ;
      buffer.(i).(t.y + t.height - 1) <- (if strong then hbline else hline)
    done ;
    for i = t.y + 1 to t.y + t.height - 2 do
      buffer.(t.x).(i) <- (if strong then vbline else vline) ;
      buffer.(t.x + t.width - 1).(i) <- (if strong then vbline else vline)
    done
end

module InputPanel = struct
  open Panel

  (** [max_char] is the maximum length of the input text accepted by the panel *)
  let max_char = 60

  type t =
    { base: Panel.t
    ; buffer: Buffer.t
    ; mutable cursor: int
    ; mutable length: int
    ; callback: string -> unit
    ; hidden: bool }

  let make x y width height hidden callback =
    assert (height > 2) ;
    { base= Panel.make x y width height
    ; buffer= Buffer.create width
    ; cursor= 0
    ; length= 0
    ; callback
    ; hidden }

  (** [draw_input t buffer] draws the input text of [t] onto [buffer] *)
  let draw_input t buffer =
    (* TODO: overflow *)
    Buffer.to_seqi t.buffer
    |> Seq.iter (fun (i, c) ->
        buffer.(t.base.x + 1 + i).(t.base.y + 1) <-
          String.make 1 (if t.hidden then '*' else c))

  let draw t buffer strong =
    draw_border t.base buffer strong ;
    draw_input t buffer ;
    set_cursor 5 5

  let get_cursor t = (t.base.x + 2 + t.cursor, t.base.y + 2)

  (** [add buffer i length c] adds [c] to [buffer] with length [length] at
   * position [i] *)
  let add buffer i length c =
    if i = length then Buffer.add_char buffer c
    else
      let temp = Buffer.sub buffer i (length - i) in
      Buffer.truncate buffer (i + 1) ;
      Buffer.add_char buffer c ;
      Buffer.add_string buffer temp

  (** [remove buffer i length] removes the character at position [i] in [buffer]
   * which has length [length] *)
  let remove buffer i length =
    if i = length - 1 then Buffer.truncate buffer (length - 1)
    else
      let temp = Buffer.sub buffer (i + 1) (length - i - 1) in
      Buffer.truncate buffer i ;
      Buffer.add_string buffer temp

  let update t input =
    match input with
    | Char c ->
      (* TODO: What do we do if the message length is larger than box width? *)
      if max_char <= t.length then return ()
      else (
        add t.buffer t.cursor t.length c ;
        t.cursor <- t.cursor + 1 ;
        t.length <- t.length + 1 ;
        return () )
    | Backspace ->
      if t.cursor > 0 then (
        remove t.buffer (t.cursor - 1) t.length ;
        t.length <- t.length - 1 ;
        t.cursor <- t.cursor - 1 ) ;
      return ()
    | Delete ->
      if t.cursor < t.length then (
        remove t.buffer t.cursor t.length ;
        t.length <- t.length - 1 ) ;
      return ()
    | Left ->
      if t.cursor > 0 then t.cursor <- t.cursor - 1 ;
      return ()
    | Right ->
      if t.cursor < t.length then t.cursor <- t.cursor + 1 ;
      return ()
    | Enter ->
      let msg = Buffer.contents t.buffer in
      (* TODO: jank *)
      t.callback msg ;
      Buffer.clear t.buffer ;
      t.cursor <- 0 ;
      t.length <- 0 ;
      return ()
    | Up | Down | Null | Escape ->
      return ()
    | CtrlL | CtrlI | CtrlS ->
      return ()

  let get_input t = Buffer.contents t.buffer
end

module MessagePanel = struct
  include Panel

  type t = {base: Panel.t; logs: Client.logs}

  let make x y width height =
    let logs = Hashtbl.create 5 in
    (* TODO: add logs for default *)
    Hashtbl.add logs (get_selected ()) DoublyLinkedList.empty ;
    ({base= Panel.make x y width height; logs}, logs)

  (** [Break] is thrown whenever we want to break out of a loop. Thank Ocaml for
   * not having basic primitive keywords. *)
  exception Break

  let draw t buffer strong =
    Panel.draw_border t.base buffer strong ;
    let u =
      match Hashtbl.find_opt t.logs (get_selected ()) with
      | Some u ->
        u
      | None ->
        Hashtbl.add t.logs (get_selected ()) DoublyLinkedList.empty ;
        Hashtbl.find t.logs (get_selected ())
    in
    let current = ref u in
    if DoublyLinkedList.is_empty !current then ()
    else
      try
        for i = 1 to t.base.height - 2 do
          (* TODO: overflow *)
          let p = DoublyLinkedList.get_value !current in
          (* TODO: don't use String.to_seqi *)
          let print_list = Parser.output_list p in
          List.iteri
            (fun j c ->
               buffer.(j + 1 + t.base.x).(t.base.y + t.base.height - i - 1) <- c)
            print_list ;
          match DoublyLinkedList.prev_opt !current with
          | Some t ->
            current := t
          | None ->
            raise Break
        done
      with Break -> ()
end

module TextPanel = struct
  type t = {x: int; y: int; mutable text: form_message list}

  let make x y text = {x; y; text}

  let set_text t text = t.text <- text

  (* TODO: overflow *)
  let draw t buffer =
    List.iteri
      (fun j c -> buffer.(j + 1 + t.x).(t.y) <- c)
      (t.text |> message_to_string)
end

module StatusPanel = struct
  include Panel

  (** [Break] is thrown whenever we want to break out of a loop. Thank Ocaml for
   * not having basic primitive keywords. *)
  exception Break

  type t = {base: Panel.t; users: string list ref; mutable selected: int}

  let make x y width height =
    let pointer = ref [] in
    ({base= Panel.make x y width height; users= pointer; selected= 0}, pointer)

  (** [update_passive t list] updates the list of users in [t] to [list] *)
  let update_passive t lst = t.users := lst

  let draw t buffer active =
    Panel.draw_border t.base buffer active ;
    let p = ref !(t.users) in
    try
      for j = t.base.y + 1 to t.base.y + t.base.height - 2 do
        match !p with
        | hd :: tl ->
          String.to_seqi hd
          |> Seq.iter (fun (i, c) ->
              buffer.(i + t.base.x + 1).(j) <- String.make 1 c) ;
          if hd = get_selected () then (
            buffer.(t.base.x + 1).(j) <-
              "\x1b[7m" ^ buffer.(t.base.x + 1).(j) ;
            buffer.(t.base.x + t.base.width - 1).(j) <-
              "\x1b[0m" ^ buffer.(t.base.x + t.base.width - 1).(j) ) ;
          p := tl
        | [] ->
          raise Break
      done
    with Break -> ()

  let get_cursor t = (t.base.x + 2, t.base.y + t.selected + 2)

  let update_active t k =
    let len = List.length !(t.users) in
    if t.selected < 0 then t.selected <- 0
    else if t.selected >= len && len > 0 then t.selected <- len - 1 ;
    match k with
    | Up | Char 'k' ->
      if t.selected > 0 then t.selected <- t.selected - 1 ;
      return ()
    | Down | Char 'j' ->
      if t.selected < len - 1 then t.selected <- t.selected + 1 ;
      return ()
    | Enter | Char 'o' ->
      if len > 0 then select_user (List.nth !(t.users) t.selected) ;
      return ()
    | _ ->
      return ()
end
