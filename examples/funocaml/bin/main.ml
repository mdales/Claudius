open Claudius

let slides = [
  Tccday3extra.slide;
  Textslide.generate_slide Textslide.opening;
  Scrollerslide.generate_slide "Tiny Code Chrismas 2022";
  Scrollerslide.generate_slide "Tiny Code Chrismas 2023";
  Tccday2.slide;
  Tccday5.slide;
  Textslide.generate_slide Textslide.code_example;
  Tccday8.slide;
  Tccday11.slide;
  Scrollerslide.generate_slide "Tiny Code Chrismas: Extra";
  Scrollerslide.generate_slide "Genuary 2024";
]

let palette_offsets s =
  let rec loop acc tot rem =
    match rem with
    | [] -> acc
    | x :: xs ->
      let updated = (x + tot) :: acc in
      loop updated (x + tot) xs
  in
    List.rev(loop [0] 0 s)

let offsets = palette_offsets (List.map (fun (p, _, _) -> (Palette.size p)) slides)

let slide_index = ref 0
let debounce = ref []

let master_boot s =
  let _p, b, _t = List.nth slides !slide_index in
  let paloff = (List.nth offsets !slide_index) in
  b s |> Framebuffer.shader (fun p -> p + paloff)

let master_tick t s prev i =
  let i_list = Base.KeyCodeSet.to_list i in
  let prev_paloff = (List.nth offsets !slide_index) in
  let updated_index = match !debounce, i_list with
  | [0x00000020], [] -> ((!slide_index) + 1) mod (List.length slides)
  | [0x4000004F], [] -> ((!slide_index) + 1) mod (List.length slides)
  | [0x40000050], [] -> ((!slide_index) - 1) mod (List.length slides)
  | _ -> !slide_index
  in
  let updated_index = if updated_index < 0 then (updated_index + (List.length slides)) else updated_index in
  let new_slide = updated_index != !slide_index in
  slide_index := updated_index;
  debounce := i_list;
  let p, b, tick = List.nth slides !slide_index in
  let paloff = (List.nth offsets !slide_index) in
  let w, h = Screen.dimensions s in
  let scale = Screen.scale s in
  let childscreen = match Screen.font s with
    | None -> Screen.create w h scale p
    | Some font -> Screen.create_with_font w h scale font p
  in
  let fb = match new_slide with
  | false -> Framebuffer.shader (fun p -> p - prev_paloff) prev
  | true -> b childscreen
  in
  tick t childscreen fb i |> Framebuffer.shader (fun p -> p + paloff)

let () =
  match Font.load_psf_font "thirdparty/tamzen-font/psf/TamzenForPowerline10x20.psf" with
  | Error (reason) -> Printf.printf "Failed to read: %s" reason
  | Ok font -> (
    Palette.of_list (List.concat_map (fun (p, _, _) -> Palette.to_list p) slides) |>
    Screen.create_with_font 640 480 2 font |>
    Base.run "Fun OCaml 2024" (Some master_boot) master_tick
  )
