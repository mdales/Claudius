open Claudius
open Bdfparser

let opening = [
  "Learning OCaml with Tiny Code Christmas & Genuary";
  "Fun OCaml 2024";
  "";
  "Michael Dales";
  "michael@digitalflapjack.com";
  "";
  "https://github.com/mdales/tcc23";
  "https://github.com/mdales/claudius";
  "https://github.com/mdales/bdfparser";
  "https://github.com/mdales/pcxlib";
  "";
  "https://tcc.lovebyte.party";
  "https://github.com/jonathanhogg/flitter";
]

let claudius_slide = [
  "Claudius overview";
  "Functional fantasy console library";
  "Flitter meets TIC-80";
  "";
  "Key types/concepts:";
  "* Screen.t - record for dimensions, palette, etc.";
  "* Palette.t - list of colours used in final render.";
  "* Framebuffer.t - the abstract framebuffer in palette space";
  "* Primatives.t - operations to render to the framebuffer";
  "";
  "Idioms:";
  "* Pixel functional - like shaders";
  "* Screen functional - transforming data to primatives";
  "* Imperative - sometimes it's just what you need";
  "";
  "Challenges:";
  "* What's a useful set of features";
  "* Immutability vs inplace";
  "* Is state a library problem or not";
  "* Where to keep up the facade";
  "";
]

let summary_slide = [
  "Summary";
]

let primatives_slide = [
  "Primatives.mli";
  "(** Primatives are a way to build up a list of rendering operations for the framebuffer ";
  "    in a functional style and then render them at once. *)";
  "";
  "type point = {";
  "    x : int ;";
  "    y : int ;";
  "}";
  " ";
  "type t =";
  "| Circle of point * float * int";
  "| FilledCircle of point * float * int";
  "| Line of point * point * int";
  "| Pixel of point * int";
  "| Polygon of point list * int";
  "| FilledPolygon of point list * int";
  "| Rect of point * point * int";
  "| FilledRect of point * point * int";
  "| Triangle of point * point * point * int";
  "| FilledTriangle of point * point * point * int";
  "| Char of point * Font.t * char * int";
  "| String of point * Font.t * string * int";
]

let tcc1_lua_example = [
  "Day 2 code in Lua";
  "";
  "w=240";
  "h=136";
  "";
  "b=120";
  "t=0";
  "";
  "snow_count=100";
  "snow={}";
  "";
  "function BOOT()";
  "  for i=1,snow_count do";
  "    snow[i]={math.random() * w, math.random() * b}";
  "  end";
  "end";
  "";
  "function TIC()";
  "  -- world";
  "  cls(8)";
  "  rect(0, b, w, h-b, 12)";
  "";
  "  -- pot";
  "  tri((w/2)+20, b-20, (w/2)-10, b, (w/2)-20, b-20, 2)";
  "  tri((w/2)-10, b, (w/2)+10, b, (w/2)+20, b-20, 2)";
  "";
  "  -- trunk";
  "  rect((w/2)-5, b-50, 10, 30, 3)";
  "";
  "  -- leaves";
  "  for i=0, 4 do";
  "    d = ((i + 2) * 5) + (i * 10)";
  "    r = (i + 1) * 7";
  "    tri((w/2), d, (w/2)-r, d+20, (w/2)+r, d+20, 7)";
  "  end";
  "";
  "  -- star";
  "  tri((w/2), 5, (w/2)-7, 15, (w/2)+7, 15, 3 + ((t/20)%2))";
  "  tri((w/2), 20, (w/2)-7, 10, (w/2)+7, 10, 3 + ((t/20)%2))";
  "";
  "  -- snow";
  "  for i = 1, snow_count do";
  "    x=snow[i][1]";
  "    y=snow[i][2]";
  "    pix(x, y, 12)";
  "    y = (y + 1) % b";
  "    if y<1 then";
  "      -- if we hit the ground,";
  "      -- make a new flake";
  "      x = math.random() * w";
  "      snow[i][1] = x";
  "    end";
  "    snow[i][2] = y";
  "  end";
  "";
  "  t = t + 1";
  "end";
]

let tcc1_code_example = [
  "Day 1 code in OCaml";
  "open Graphics";
  "";
  "let draw_background () = ";
  "  let horizon_height = (480 / 3) in";
  "    set_color white;";
  "    fill_rect 0 0 640 horizon_height;";
  "    set_color blue;";
  "    fill_rect 0 horizon_height 640 (480  - horizon_height)";
  "";
  "let draw_stars () = ";
  "  set_color white;";
  "  for i = 0 to 480 do";
  "    plot (Random.int 640) i";
  "  done";
  "";
  "let draw_tree x y =";
  "  (* pot *)";
  "  set_color red;";
  "  let pot_height = 70 in";
  "    let pot_upper_radius = 50 in ";
  "      let pot_lower_radius = 30 in";
  "        fill_poly [| ";
  "          (x + pot_lower_radius, y); ";
  "          (x + pot_upper_radius, y + pot_height); ";
  "          (x - pot_upper_radius, y + pot_height); ";
  "          (x - pot_lower_radius, y) ";
  "        |]; ; ; ";
  "    (* trunk *)";
  "    set_color (rgb 0xC0 0x80 0x20);";
  "    let trunk_height = 50 in";
  "      let trunk_radius = 20 in";
  "          fill_rect (x - trunk_radius) (y + pot_height) (trunk_radius * 2) (trunk_height); ;";
  "      (* leaves *)";
  "      set_color (rgb 0x20 0xA0 0x20);";
  "      let tree_height = 250 in";
  "        let tiers = 4 in";
  "          let tree_radius = 100 in";
  "              for i = 0 to tiers do";
  "              (* let i = 0 in *)";
  "                fill_poly [| ";
  "                  ((x + tree_radius) + ((tiers - i) * 15), y + pot_height + trunk_height + (i * 30)); ";
  "                  (x, y + pot_height + trunk_height + tree_height - ((tiers - i) * 30));";
  "                  ((x - tree_radius) - ((tiers - i) * 15), y + pot_height + trunk_height + (i * 30)) ";
  "                |]";
  "              done; ; ;";
  "        (* star *)";
  "        set_color yellow;";
  "        let star_offset = (y + pot_height + trunk_height + tree_height) in";
  "          fill_poly [| (x, star_offset - 20); (x + 15, star_offset + 10); (x - 15, star_offset + 10) |];";
  "          fill_poly [| (x, star_offset + 20); (x + 15, star_offset - 10); (x - 15, star_offset - 10) |]";
  "";
  "let draw_scene () = ";
  "  draw_background ();";
  "  draw_stars ();";
  "  draw_tree 320 60";
  "  ";
  "let () = ";
  "  auto_synchronize false;";
  "  display_mode false;";
  "  remember_mode true;";
  "  (* The space here before the res is important :/ *)";
  "  open_graph \" 640x480\";";
  "  draw_scene ();";
  "  synchronize ();";
  "  ignore (wait_next_event [ Key_pressed ])";
]

let tcc5_code_example = [
  "Day 5 code";
  "let tick (t : int) =";
  "  (* Generate some useful consts - type safety is not good for sizecoding! *)";
  "  let ft = float_of_int t";
  "  and height = size_y () and width = size_x () ";
  "  and colors = (List.length palette) in";
  "  let fcolors = float_of_int colors in";
  "  (* This effect is pixel functional, so we can do it in the buffer init function! *)";
  "  for j = 0 to height do";
  "    for i = 0 to width do";
  "      (* offset to middle of screen *)";
  "      let x = float_of_int (i - (width / 2))";
  "      and y = Float.of_int (j - (height / 2)) in";
  "";
  "      let d1 = (float_of_int width) /. sqrt ((x *. x) +. (y *. y) +. 1.0)";
  "      and c1 = ((atan2 y x) +. Float.pi) *. (fcolors /. (2.0 *. Float.pi)) in";
  "";
  "      let c2 = c1 +. (sin (ft /. 70.0) *. Float.pi *. 2.0)";
  "      and d2 = d1 +. (Float.rem (ft /. 10.0) fcolors) in";
  "";
  "      let p = (int_of_float (Float.floor c2)) lxor (int_of_float (Float.floor d2)) in";
  "";
  "      let pindex = (p mod colors) in";
  "      let color = List.nth palette (if pindex < 0 then (colors + pindex) else pindex) in";
  "      set_color color; plot i j";
  "    done";
  "  done;";
  "  set_color (List.nth palette 1);";
  "  fill_circle (width / 2) (height / 2) 15";
  "";
  "let inner_tick (t : int) =";
  "    tick t;";
  "    synchronize ()";
  "";
  "let () =";
  "  open_graph \" 640x480\";";
  "  set_window_title \"TCC Day 5\";";
  "  auto_synchronize false;";
  "";
  "  let t = ref 0 in";
  "  while true do";
  "    (* Unix.sleepf 0.05; *)";
  "    let status = wait_next_event[ Poll; Key_pressed ] in";
  "    if status.keypressed && status.key == ' ' then ";
  "      raise Exit ";
  "    else";
  "      inner_tick !t;";
  "      t := !t + 1;";
  "  done";
]

let genuary1_code_example = [
  "Particals code";
  "let tick (t : int) (s : Screen.t) (prev : Framebuffer.t) (_inputs : Base.KeyCodeSet.t): Framebuffer.t =";
  "  let buffer = Framebuffer.init (Screen.dimensions s) (fun _ -> 0)";
  " ";
  "  let ft = Float.of_int t in";
  "";
  "  generate_planet_points";
  "  |> List.map (fun p ->";
  "    rotate_y (0.02 *. ft) p |> rotate_x (0.01 *. ft) |> rotate_z (0.005 *. ft)";
  "  )";
  "  |> List.sort point_z_cmp";
  "  |> render_to_primitives ft s";
  "  |> Framebuffer.render buffer;";
  "";
  "  buffer";
  "";
  "(* ----- *)";
  "";
  "let () =";
  "  Palette.generate_mono_palette 16";
  "  |> Screen.create 640 480 1";
  "  |> Base.run \"Genuary Day 1: Particals\" None tick";
]

let genuary2_code_example = [
  "No Palette code";
  "open Claudius";
  "";
  "let tick t s _prev _inputs =";
  "  let palsize = Palette.size (Screen.palette s) in";
  "  Framebuffer.init (Screen.dimensions s) (fun x y -> ";
  "      let ft = (Float.of_int t) /. 10. and fx = (Float.of_int x) /. 140. ";
  "      and fy = (Float.of_int y) /. 140. in";
  "      let z = 10. +. (sin (ft /. 1000.) *. 5.)";
  "      and d = 10. +. (cos (ft /. 1000.) *. 5.) in";
  "      let fc = (sin (sin ((fx +. ft) /. z)) +. sin (sin ((fy +. ft) /. d))) *. ";
  "          Float.of_int(palsize / 2) in";
  "      let rc = ((int_of_float fc)) mod palsize in";
  "      if rc >= 0 then rc else (rc + palsize)";
  "  )";
  "";
  "let () =";
  "  Palette.generate_plasma_palette 1024 |>";
  "  Screen.create 640 480 1 |>";
  "  Base.run \"Genuary Day 2: No Palette\" None tick";
]

let genuary6_code_example = [
  "Screen Saver code";
  "open Claudius";
  "";
  "let tails = 50";
  "";
  "let triangle (x : float) (amplitude : float) (period : float) : float =";
  "((2. *. amplitude) /. Float.pi) *. asin (sin (x *. ((2. *. Float.pi) /. period)))";
  "";
  "let point d ft =";
  "  let fd = Float.of_int d in";
  "  (d / 2) + Int.of_float (triangle (ft +. Random.float 50.) (fd /. 2.) (50. +. Random.float 100.))";
  "";
  "let line t s col =";
  "  Random.init 789123;";
  "  let ft = Float.of_int t in";
  "  let w, h = Screen.dimensions s in";
  "  Primitives.Line (";
  "    { x = point w ft ; y = point h ft },";
  "    { x = point w ft ; y = point h ft },";
  "    col";
  "  )";
  "";
  "let tick t s p _i =";
  "  Framebuffer.render p (";
  "    (if t >= tails then [(line (t - tails) s 0)] else []) @";
  "    [line t s ((t mod ((Palette.size (Screen.palette s)) - 1)) + 1)]";
  "  );";
  "  p";
  "";
  "let () =";
  "Palette.of_list (0x000000 :: (List.rev (Palette.to_list (Palette.generate_plasma_palette 31)))) |>";
  "  Screen.create 512 384 1 |>";
  "  Base.run \"Genuary Day 6: Screen saver\" None tick";
]

let palette =  Palette.load_tic80_palette "000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57"
let _palette =
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

let offset = ref 0
let debounce = ref []

let boot s =
  offset := 0;
  Framebuffer.init (Screen.dimensions s) (fun _x _y ->  15)

let tick title_font body_font prose _t s _fb i =

  let i_list = Base.KeyCodeSet.to_list i in
  let updated_offset = match !debounce, i_list with
  | [0x00000020], [] -> (!offset) + 1
  | [0x40000051], [] -> (!offset) + 1
  | [0x40000052], [] -> (!offset) - 1
  | _ -> !offset
  in
  offset := updated_offset;
  debounce := i_list;

  let fb = Framebuffer.init (Screen.dimensions s) (fun _x _y ->  0) in
  let w, h = Screen.dimensions s in

  let palsize = Palette.size (Screen.palette s) in

  let step = h / palsize in
  for i = 0 to palsize do
    Framebuffer.filled_rect (w - 16) (step * i) 16 step (palsize - (i + 1)) fb
  done;

  List.iteri (fun i s ->

    let font = match i with
    | 0 -> title_font
    | _ -> body_font in

    let inset = 5 in

    let trimmed = String.trim s in
    let comment = ((String.starts_with ~prefix:"(*" trimmed ) && (String.ends_with ~suffix:"*)" trimmed )) ||
      (String.starts_with ~prefix:"--" trimmed) in

    let col = match comment with
    | false -> 12
    | true -> 13
    in
    ignore(draw_string inset ((i + 1 - !offset) * 17) font s  col fb)

  ) prose;

  fb

let generate_slide prose =
  let body_font = Result.get_ok (Bdf.create "/Users/michael/Dev/classic-mac-fonts/bdf/Courier-12.bdf")
  and title_font = Result.get_ok (Bdf.create "/Users/michael/Dev/chicago-bdf/Chicago-12.bdf") in
  (palette, Some boot, tick title_font body_font prose)
