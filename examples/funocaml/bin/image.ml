open Claudius
open Giflib

let tick gif palette_offsets t s p _i =
  if (t mod 10) == 0 then (
    let c = GIF.image_count gif in
    let i = GIF.get_image gif ((t / 10) mod c) in

    let po = palette_offsets.((t / 10) mod c) in

    let sw, sh = Screen.dimensions s
    and gw, gh = GIF.dimensions gif
    and iw, ih = Image.dimensions i
    and ixoff, iyoff = Image.offset i
    and pixels = Image.pixels i in
    let transparent = match (Image.transparent i) with
      | None -> -1
    | Some x -> x
    in
    let sxoff = (sw - gw) / 2
    and syoff = (sh - gh) / 2 in
    for x = 0 to (iw - 1) do
      for y = 0 to (ih - 1) do
        let v = pixels.(x + (y * iw)) in
        if (v != transparent) then
        Framebuffer.pixel_write (x + sxoff + ixoff) (y + syoff + iyoff) (v + po) p
      done
    done;
  );
  p

let generate_slide filename =

  let gif = GIF.from_file filename in
  let frames = GIF.image_count gif in

  let palettes = Array.init frames (fun idx ->
    let i = GIF.get_image gif idx in
    List.map (fun (r, g, b) ->
      b + (g * 256) + (r * 256 * 256)
    ) (Array.to_list (Image.palette i)))
  in

  let palette = List.flatten (Array.to_list palettes) |> Palette.of_list in

  let palette_offset = Array.init frames (fun _ -> 0) in
  for idx = 1 to (frames - 1) do
    palette_offset.(idx) <- palette_offset.(idx - 1) + (List.length palettes.(idx - 1))
  done;

  (palette, None, tick gif palette_offset)
