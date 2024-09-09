open Claudius

let slides: ((Palette.t * (Screen.t -> Framebuffer.t) option * (int -> Screen.t -> Framebuffer.t -> Base.KeyCodeSet.t -> Framebuffer.t)) * string option) list = [


  (Textslide.generate_slide Textslide.opening, None);

  (Quote.generate_slide Quote.wells_quote, None);

  (Scrollerslide.generate_slide "TIC-80 and friends", None);
  (Image.generate_slide "examples/funocaml/resources/smalldemo.gif", None);
  (* (Tic80hello.generate_slide (), None);*)


  (Scrollerslide.generate_slide "Tiny Code Chrismas", None);
  (Tccday2.slide, Some "Day 1");
  (Code.generate_slide "../tcc22/day1.lua" 7, None);
  (Code.generate_slide "../tcc23/day1/bin/main.ml" 0, None);
  (Tccday5.slide, Some "Day 5");
  (Code.generate_slide "../tcc23/day5/bin/main.ml" 30, None);
  (Tccday8.slide, Some "Day 8");
  (Tccday11.slide, Some "Day 11");
  (Tccday12.slide, Some "Day 12");

  (Scrollerslide.generate_slide "Tiny Code Chrismas: Extra", None);
  (Tccday2extra.slide, Some "Day 2 extra");
  (Tccday3extra.slide, Some "Day 3 extra");
  (Tccday8extra.slide, Some "Day 8 extra");
  (Code.generate_slide "../tcc23/day8extra/bin/main.ml" 81, None);


  (Image.generate_slide "examples/funocaml/resources/flitter.gif", None);
  (Image.generate_slide "/Users/michael/Desktop/claudius.gif", None);
  (Scrollerslide.generate_slide "Genuary 2024", None);
  (Prompts.slide, None);
  (Textslide.generate_slide Textslide.claudius_slide, None);

  (Quote.generate_slide Quote.lynch_quote, None);

  (Genuary2.slide, Some "No Palette");
  (Code.generate_slide "examples/day2/bin/main.ml" 0, None);
  (Genuary1.slide, Some "Particals");
  (Code.generate_slide "examples/day1/bin/main.ml" 100, None);
  (Genuary6.slide, Some "Screen saver");
  (* (Textslide.generate_slide Textslide.genuary6_code_example, None);*)
  (Code.generate_slide "claudius/primitives.mli" 0, None);
  (Genuary17.slide, Some "Islamic Patterns");
  (Genuary17filled.slide, Some "Islamic Patterns");
  (Genuary20.slide, Some "Generative Typography and 8x8");
  (Genuary16.slide, Some "Use a physics engine");

  (Scrollerslide.generate_slide "Summary", None);
  (Textslide.generate_slide Textslide.summary_slide, None);
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
