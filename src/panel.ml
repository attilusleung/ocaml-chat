open Unix
open ANSITerminal

(*
   module type Panel = sig
     type t
     val make: int -> int -> int -> int -> t
     val draw_border: t -> string array array -> bool -> unit
   end
*)

module Panel = struct
  type t = { x:int
           ; y:int
           ; width:int
           ; height:int}

  (* h: horizontal
   * v: vertical
   * u: upper
   * l: lower
   * l: left
   * r: right
   * b: bold *)
  (* Where is unicode escapes in the Ocaml Documentation? *)
  let hline = "\u{2500}"
  let hbline = "\u{2501}"
  let vline = "\u{2502}"
  let vbline = "\u{2503}"
  let ulcorner = "\u{250C}"
  let ulbcorner = "\u{250F}"
  let urcorner = "\u{2510}"
  let urbcorner = "\u{2513}"
  let llcorner = "\u{2514}"
  let llbcorner = "\u{2517}"
  let lrcorner = "\u{2518}"
  let lrbcorner = "\u{251B}"

  let make x y width height =
    assert (x >= 0) ;
    assert (y >= 0) ;
    assert (width > 1);
    assert (height > 1);
    {x= x; y= y; width= width; height= height}

  let draw_border t buffer strong =
    buffer.(t.x).(t.y) <- if strong then ulbcorner else ulcorner;
    buffer.(t.x).(t.y + t.height - 1) <- if strong then llbcorner else llcorner;
    buffer.(t.x + t.width - 1).(t.y) <- if strong then urbcorner else urcorner;
    buffer.(t.x + t.width - 1).(t.y + t.height - 1) <- if strong then lrbcorner
      else lrcorner;
    for i = t.x + 1 to t.x + t.width - 2 do
      buffer.(i).(t.y) <- if strong then hbline  else hline;
      buffer.(i).(t.y + t.height - 1) <- if strong then hbline else hline;
    done;
    for i = t.y + 1 to t.y + t.height - 2 do
      buffer.(t.x).(i) <- if strong then vbline else vline;
      buffer.(t.x + t.width - 1).(i) <- if strong then vbline else vline;
    done
end

module InputPanel = struct
  open Panel
  type t = { base: Panel.t
           ; buffer: Buffer.t
           ; }

  let make x y width height =
    assert (height > 2);
    { base= Panel.make x y width height
    ; buffer= Buffer.create width }

  let draw_input t buffer =
    Buffer.to_seqi t.buffer
    |> Seq.iter
      (fun (i, c) -> buffer.(t.base.x + 1 + i).(t.base.y+1) <- String.make 1 c)

  let draw t buffer strong =
    draw_border t.base buffer strong;
    draw_input t buffer

  let update t input =
    if input > 127 then () else
      Buffer.add_char t.buffer @@ char_of_int input

end
