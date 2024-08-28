open Claudius

let prompts = [
  "lauriej";
  "pferris";
  "dave84";
  "catnip";
  "aldroid";
  "avsm";
  "nickludlam";
  "jonathanhogg";
  "enfys";
  "genuary.art";
  "glasgowdave";
]

let boot s = Framebuffer.init (Screen.dimensions s) (fun _ _ -> 10)

let tick t s prev _i =
  let w, h = Screen.dimensions s in
  let buffer = Framebuffer.shader (fun pixel ->
    if (pixel < 32) then pixel else (pixel - 1)
  ) prev in

  let font = Option.get (Screen.font s) in

  if (t mod 50) == 0 then (
    let idx = (t / 50) mod (List.length prompts) in
    let prose = List.nth prompts idx in
    let prose_w = (Framebuffer.draw_string 10 (0-100) font prose 255 buffer) in
    let bounds = w - prose_w in
    ignore(Framebuffer.draw_string (Random.int bounds) (Random.int (h - 10)) font prose 255 buffer)
  );

  buffer

let palette =  (Palette.generate_plasma_palette 256)

let slide = (palette, Some boot, tick)
