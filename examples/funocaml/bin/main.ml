open Claudius

let slides: ((Palette.t * (Screen.t -> Framebuffer.t) option * (int -> Screen.t -> Framebuffer.t -> Base.KeyCodeSet.t -> Framebuffer.t)) * string option) list = [
  (Textslide.generate_slide Textslide.opening, None);
  (Scrollerslide.generate_slide "Tiny Code Chrismas 2022", None);
  (Image.generate_slide "/Users/michael/Desktop/tic80.pcx", None);
  (Scrollerslide.generate_slide "Tiny Code Chrismas 2023", None);
  (Tccday2.slide, Some "Day 1");
  (Textslide.generate_slide Textslide.tcc1_lua_example, None);
  (Textslide.generate_slide Textslide.tcc1_code_example, None);
  (Tccday5.slide, Some "Day 5");
  (Textslide.generate_slide Textslide.tcc5_code_example, None);
  (Tccday8.slide, Some "Day 8");
  (Tccday11.slide, Some "Day 11");
  (Tccday12.slide, Some "Day 12");
  (Scrollerslide.generate_slide "Tiny Code Chrismas: Extra", None);
  (Tccday2extra.slide, Some "Day 2 extra");
  (Tccday3extra.slide, Some "Day 3 extra");
  (Tccday8extra.slide, Some "Day 8 extra");
  (Scrollerslide.generate_slide "Genuary 2024", None);
  (Prompts.slide, None);
  (Quote.slide, None);
  (Image.generate_slide "/Users/michael/Desktop/claudius.pcx", None);
  (Textslide.generate_slide Textslide.claudius_slide, None);
  (Textslide.generate_slide Textslide.primatives_slide, None);
  (Genuary1.slide, Some "1: Particals");
  (Genuary2.slide, Some "2: No Palette");
  (Textslide.generate_slide Textslide.genuary2_code_example, None);
  (Genuary6.slide, Some "6: Screen saver");
  (Textslide.generate_slide Textslide.genuary6_code_example, None);
  (Genuary16.slide, Some "15: Use a physics engine");
  (Genuary17.slide, Some "17: Islamic Patterns");
  (Genuary17filled.slide, Some "17: Islamic Patterns");
  (Genuary20.slide, Some "20 & 23: Generative Typography and 8x8");
  (Credits.slide, None);
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
  let ((_p, boot, _t), _) = List.nth slides !slide_index in
  let paloff = (List.nth offsets !slide_index) in
  (match boot with
  | None -> Framebuffer.init (Screen.dimensions s) (fun _x _y -> 0)
  | Some b -> b s ) |> Framebuffer.shader (fun p -> p + paloff)

let counter = ref 1.0
let last_fps = ref 0.0
let prevt = ref (Unix.gettimeofday ())
let toffset = ref 0

let master_tick t s _prev i =
  let i_list = Base.KeyCodeSet.to_list i in
  let updated_index = match !debounce, i_list with
  | [0x4000004F], [] -> ((!slide_index) + 1) mod (List.length slides)
  | [0x40000050], [] -> ((!slide_index) - 1) mod (List.length slides)
  | _ -> !slide_index
  in
  let updated_index = if updated_index < 0 then (updated_index + (List.length slides)) else updated_index in
  let new_slide = updated_index != !slide_index in
  slide_index := updated_index;
  debounce := i_list;
  let (p, boot, tick), overlay = List.nth slides !slide_index in
  let paloff = (List.nth offsets !slide_index) in
  let w, h = Screen.dimensions s in
  let scale = Screen.scale s in
  let childscreen = match Screen.font s with
    | None -> Screen.create w h scale p
    | Some font -> Screen.create_with_font w h scale font p
  in
  let fb = match new_slide with
  | false -> !in_colour_space_prev
  | true -> (
    toffset := t;
    match boot with
    | None -> Framebuffer.init (Screen.dimensions s) (fun _x _y -> 0)
    | Some b -> b childscreen
  )
  in
  let rendered = tick (t - !toffset) childscreen fb i in
  in_colour_space_prev := rendered;
  let final = Framebuffer.shader (fun p -> p + paloff) rendered in
  (match overlay with
  | None -> ()
  | Some prose -> ignore (Textslide.draw_string 50 50 overlay_font prose 12 final));

  let nowt = Unix.gettimeofday () in
  counter := (!counter) +. (nowt -. !prevt);
  prevt := nowt;
  if (t mod 100) == 0 then (
    last_fps := 100. /. !counter;
    counter := 0.0
  );
  let show_fps = Base.KeyCodeSet.exists (fun x -> x == 0x00000066) i in
  if show_fps then (
    let fpss = Printf.sprintf "%d fps" (int_of_float !last_fps) in
    ignore (Textslide.draw_string 50 450 overlay_font fpss 12 final);
  );

  final

let () =
  match Font.load_psf_font "thirdparty/tamzen-font/psf/TamzenForPowerline10x20.psf" with
  | Error (reason) -> Printf.printf "Failed to read: %s" reason
  | Ok font -> (
    let s = Palette.of_list (List.concat_map (fun ((p, _, _), _) -> Palette.to_list p) slides) |>
      Screen.create_with_font 640 480 2 font in
    let (_, boot, _), _ = List.nth slides 0 in
    in_colour_space_prev := (
      match boot with
      | None -> Framebuffer.init (Screen.dimensions s) (fun _x _y -> 0)
      | Some b -> b s
    );
    Base.run "Fun OCaml 2024" (Some master_boot) master_tick s
  )
