open Claudius

let slides = [
  (Textslide.generate_slide Textslide.opening, None);
  (Scrollerslide.generate_slide "Tiny Code Chrismas 2022", None);
  (Scrollerslide.generate_slide "Tiny Code Chrismas 2023", None);
  (Tccday2.slide, Some "Day 2");
  (Tccday5.slide, Some "Day 5");
  (Textslide.generate_slide Textslide.code_example, None);
  (Tccday8.slide, Some "Day 8");
  (Tccday11.slide, Some "Day 11");
  (Scrollerslide.generate_slide "Tiny Code Chrismas: Extra", None);
  (Tccday2extra.slide, Some "Day 2 extra");
  (Tccday3extra.slide, Some "Day 3 extra");
  (Tccday8extra.slide, Some "Day 8 extra");
  (Scrollerslide.generate_slide "Genuary 2024", None);
  (Genuary16.slide, Some "16: Use a physics engine");
]

let overlay_font = Result.get_ok (
  Bdfparser.Bdf.create "/Users/michael/dev/classic-mac-fonts/bdf/Geneva-24.bdf"
)

let palette_offsets s =
  let rec loop acc tot rem =
    match rem with
    | [] -> acc
    | x :: xs ->
      let updated = (x + tot) :: acc in
      loop updated (x + tot) xs
  in
    List.rev(loop [0] 0 s)

let offsets = palette_offsets (List.map (fun ((p, _, _), _) -> (Palette.size p)) slides)

let slide_index = ref 0
let debounce = ref []

let in_colour_space_prev = ref (Framebuffer.init (640, 480) (fun _ _ -> 0))

let master_boot s =
  let ((_p, b, _t), _) = List.nth slides !slide_index in
  let paloff = (List.nth offsets !slide_index) in
  b s |> Framebuffer.shader (fun p -> p + paloff)

let master_tick t s _prev i =
  let i_list = Base.KeyCodeSet.to_list i in
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
  let (p, b, tick), overlay = List.nth slides !slide_index in
  let paloff = (List.nth offsets !slide_index) in
  let w, h = Screen.dimensions s in
  let scale = Screen.scale s in
  let childscreen = match Screen.font s with
    | None -> Screen.create w h scale p
    | Some font -> Screen.create_with_font w h scale font p
  in
  let fb = match new_slide with
  | false -> !in_colour_space_prev
  | true -> b childscreen
  in
  let rendered = tick t childscreen fb i in
  in_colour_space_prev := rendered;
  let final = Framebuffer.shader (fun p -> p + paloff) rendered in
  (match overlay with
  | None -> ()
  | Some prose -> ignore (Textslide.draw_string 50 50 overlay_font prose 0 final));
  final

let () =
  match Font.load_psf_font "thirdparty/tamzen-font/psf/TamzenForPowerline10x20.psf" with
  | Error (reason) -> Printf.printf "Failed to read: %s" reason
  | Ok font -> (
    let s = Palette.of_list (List.concat_map (fun ((p, _, _), _) -> Palette.to_list p) slides) |>
      Screen.create_with_font 640 480 2 font in
    let (_, b, _), _ = List.nth slides 0 in
    in_colour_space_prev := b s;
    Base.run "Fun OCaml 2024" (Some master_boot) master_tick s
  )
