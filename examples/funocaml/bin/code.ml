open Claudius
open Bdfparser

let ocaml_keywords = [
  "open";
  "let";
  "ref";
  "in";
  "for";
  "do";
  "done";
  "done;";
  "if";
  "then";
  "and";
  "match";
  "with";
  "exception";
]

let lua_keywords = [
  "function";
  "local";
  "for";
  "do";
  "in";
  "if";
  "then";
  "end";
]


let palette =  Palette.load_tic80_palette "000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57"

let inset = 5

let offset = ref 0
let debounce = ref []

let boot initial_offset s =
  offset := initial_offset;
  Framebuffer.init (Screen.dimensions s) (fun _x _y ->  0)

let draw_string x y font s keywords fb =
  let trimmed = String.trim s in
  let comment = ((String.starts_with ~prefix:"(*" trimmed ) && (String.ends_with ~suffix:"*)" trimmed )) ||
    (String.starts_with ~prefix:"--" trimmed) in

  let col = match comment with
  | false -> 12
  | true -> 13
  in

  let words = String.split_on_char ' ' s in
  let space_width = Textslide.draw_string 0 0 font " " 0 fb in

  List.fold_left (fun offset w ->
    let c = match col with
    | 13 -> 13
    | x -> (
      match List.exists (fun k -> String.compare k w == 0) keywords with
      | true -> 3
      | false -> (
        match int_of_string_opt w with
        | None -> x
        | Some _ -> 11
      )
    ) in
    offset + (Textslide.draw_string offset y font w c fb) + space_width
  ) x words


let tick title_font body_font heading code keywords _t s _fb i =
  let w, h = Screen.dimensions s in

  let i_list = Base.KeyCodeSet.to_list i in
  let updated_offset = match !debounce, i_list with
  | [0x00000020], [] -> (!offset) + 1
  | [0x40000051], [] -> (!offset) + 1
  | [0x40000052], [] -> (!offset) - 1
  | _ -> !offset
  in
  offset := updated_offset;
  debounce := i_list;

  let fb = Framebuffer.init (w, h) (fun _x _y ->  0) in

  let palsize = Palette.size (Screen.palette s) in



  List.iteri (fun i s ->
    ignore(draw_string inset ((i + 2 - !offset) * 17) body_font s keywords fb)
  ) code;

  let step = h / palsize in
  List.init palsize (fun i ->
    Primitives.FilledRect(
      {x = w - 16 ; y = step * i},
      {x = w - 1; y = (step * (i + 1))},
      (palsize - (i + 1))
    )
  )
  |> Framebuffer.render fb;

  Framebuffer.filled_rect 0 0 (w - 16) 18 0 fb;
  ignore(Textslide.draw_string inset 17 title_font (Printf.sprintf "%s : %d" heading !offset) 12 fb);

  fb


let generate_slide filename initial_offset =
  let code = In_channel.with_open_bin filename In_channel.input_all |>
    String.split_on_char '\n'
  in

  let fname = Fpath.of_string filename |> Result.get_ok in
  let cwd = Fpath.of_string (Sys.getcwd ()) |> Result.get_ok in
  let basename = Fpath.normalize (Fpath.append cwd fname ) |> Fpath.to_string
  and ext = Fpath.get_ext fname in

  let keywords = match ext with
  | ".ml" -> ocaml_keywords
  | ".lua" -> lua_keywords
  | _ -> []
  in

  let body_font = Result.get_ok (Bdf.create "/Users/michael/Dev/classic-mac-fonts/bdf/Courier-12.bdf")
  and title_font = Result.get_ok (Bdf.create "/Users/michael/Dev/chicago-bdf/Chicago-12.bdf") in
  (palette, Some (boot initial_offset), tick title_font body_font basename  code keywords)
