open Claudius

let prompts = [
  "Particles, lots of them.";
  "No palettes.";
  "Droste effect.";
  "Pixels.";
  "In the style of Vera Molnar (1924-2023).";
  "Screensaver.";
  "Progress bar / indicator / loading animation.";
  "Chaotic system.";
  "ASCII.";
  "Hexagonal.";
  "In the style of Anni Albers (1899-1994).";
  "Lava lamp.";
  "Wobbly function day.";
  "Less than 1KB artwork.";
  "Use a physics library.";
  "Draw 10 000 of something.";
  "Inspired by Islamic art.";
  "Bauhaus.";
  "Flocking.";
  "Generative typography.";
  "Use a library that you haven't used before.";
  "Point - line - plane.";
  "32x32.";
  "Impossible objects (undecided geometry).";
  "Cool looking patterns, textures, shapes.";
  "Grow a seed.";
  "Code for one hour. At the one hour mark, you're done.";
  "Skeuomorphism.";
  "Signed Distance Functions.";
  "Shaders.";
  "Generative music / Generative audio / Generative sound.";
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

let palette =  (Palette.generate_mono_palette 256)

let slide = (palette, Some boot, tick)
