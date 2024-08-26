open Claudius
open Bdfparser

let opening = [
  "Learning OCaml with Tiny Code Xmas";
  "Fun OCaml 2024";
  "";
  "Michael Dales";
  "michael@digitalflapjack.com";
  "";
  "https://github.com/mdales/claudius";
  "https://github.com/mdales/bdfparser";
]

let code_example = [
  "Day 5 code";
  "let tick t s _p _i =";
  "  let ft = (float_of_int t)";
  "  and w, h = (Screen.dimensions s)";
  "  and colors = (Palette.size (palette)) in";
  "  let fcolors = Float.of_int colors in";
  "  let buffer = Framebuffer.init (w, h) (fun i j ->";
  "    let x = Float.of_int (i - (w / 2))";
  "    and y = Float.of_int (j - (h / 2)) in";
  "    let d1 = (float_of_int w) /. sqrt ((x *. x) +. (y *. y) +. 1.0)";
  "    and c1 = ((atan2 y x) +. Float.pi) *. (fcolors /. (2.0 *. Float.pi)) in";
  "    let c2 = c1 +. (sin (ft /. 70.0) *. Float.pi *. 2.0)";
  "    and d2 = d1 +. (Float.rem (ft /. 10.0) fcolors) in";
  "    let p = (int_of_float (Float.floor c2)) lxor (int_of_float (Float.floor d2)) in";
  "    let pindex = (p mod colors) in";
  "    if pindex < 0 then (colors + pindex) else (pindex)";
  "  ) in";
  "  Framebuffer.filled_circle (w / 2) (h / 2) 15. 1 buffer;";
  "  buffer";
]

let palette =
  Palette.of_list [
    0x00FFFFFF;
    0x001B202B;
    0x00B2552C;
  ]

let draw_char (x : int) (y : int) (f : Bdf.t) (c : char) (col : int) (buffer : Framebuffer.t) : int =
  match Bdf.glyph_of_char f (Uchar.of_char c) with
  | None -> 0
  | Some glyph -> (
    let gw, gh, ox, oy = Bdf.Glyph.dimensions glyph in
    let bmp = Bdf.Glyph.bitmap glyph in
    (* Framebuffer.draw_rect x (y - gh) gw gh (Bytes.length bmp) buffer; *)
    if gh > 0 then (
    let bytes_per_line = (Bytes.length bmp) / gh in
    for h = 0 to (gh - 1) do
      for w = (bytes_per_line - 1) downto 0 do

        let bits_offset = ((bytes_per_line - 1) - w) * 8 in

        let bitcount = if (((bits_offset + 8)) < gw) then 8 else ((gw - bits_offset)) in
        let b = int_of_char (Bytes.get bmp ((h * bytes_per_line) + w)) in
        for bit = 0 to (bitcount - 1) do
          let isbit = (b lsl bit) land 0x80 in
          match isbit with
          | 0 -> ()
          | _ ->
          Framebuffer.pixel_write
            (x + bits_offset + bit + ox)
            ((y - oy) + h - gh)
            col
            buffer
        done
      done
    done); gw
  )

let draw_string (x : int) (y : int) (f : Bdf.t) (s : string) (col : int) (buffer : Framebuffer.t) =
  let sl = List.init (String.length s) (String.get s) in
  let rec loop offset remaining =
    match remaining with
    | [] -> offset
    | c :: remaining -> (
      let width = draw_char (x + offset) y f c col buffer in
      loop (offset + width) remaining
    )
  in loop 0 sl

let string_length (f : Bdf.t) (s : string) : int =
  let sl = List.init (String.length s) (String.get s) in
  List.fold_left (fun acc c ->
    let cwidth = match Bdf.glyph_of_char f (Uchar.of_char c) with
    | None -> 0
    | Some glyph -> (
      let gw, _gh, _ox, _oy = Bdf.Glyph.dimensions glyph in
      gw
    )
    in
    acc + cwidth
  ) 0 sl

let boot s =
  Framebuffer.init (Screen.dimensions s) (fun _x _y ->  0)

let tick title_font body_font prose _t s fb _i =
  (* let fb = Framebuffer.init (Screen.dimensions s) (fun _x _y ->  0) in*)
  let _w, _h = Screen.dimensions s in
  List.iteri (fun i s ->

    let font = match i with
    | 0 -> title_font
    | _ -> body_font in

    let _width = string_length font s in
    let inset = 5 in
    (* (w / 2) - (width / 2))  in *)

    ignore(draw_string inset ((i + 1) * 17) font s  1 fb)

  ) prose;

  fb

let generate_slide prose =
  let body_font = Result.get_ok (Bdf.create "/Users/michael/Dev/classic-mac-fonts/bdf/Geneva-12.bdf")
  and title_font = Result.get_ok (Bdf.create "/Users/michael/Dev/chicago-bdf/Chicago-12.bdf") in
  (palette, boot, tick title_font body_font prose)
