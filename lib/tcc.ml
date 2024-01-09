open Graphics

type boot_func = Screen.t -> Framebuffer.t
type tick_func = int -> Screen.t -> Framebuffer.t -> Framebuffer.t

let expanded_row (s : Screen.t) (row : int array) : color array =
  Array.concat (List.map (fun (vl : int) : color array ->
    let col = match (Palette.index_to_rgb (Screen.palette s) vl) with
    | Some x -> x
    | None -> 0
   in
      Array.make (Screen.scale s) col
    ) (Array.to_list row))

let buffer_to_image (s : Screen.t) (buffer : Framebuffer.t) : image =
  let raw = Framebuffer.to_array buffer in
  let img = Array.concat (List.map (fun (row : int array) : color array array ->
    let colrow = expanded_row s row in
      Array.make (Screen.scale s) colrow
  ) (Array.to_list raw)) in
    make_image img


(* ----- *)

let run (s : Screen.t) (title : string) (boot : boot_func option) (tick : tick_func)  =
  let width, height = Screen.dimensions s
  and scale = Screen.scale s in

  open_graph (Printf.sprintf " %dx%d" (width * scale) (height * scale));
  set_window_title title;
  auto_synchronize false;
  set_font "-*-*-bold-r-*-*-32-*-*-*-m-*-iso8859-1";

  let initial_buffer = match boot with
  | None -> Framebuffer.init (width, height) (fun _x _y -> 0)
  | Some bfunc -> bfunc s
  in

  let rec loop (t : int) (prev_buffer : Framebuffer.t) = (
    let status = wait_next_event[ Poll; Key_pressed ] in
    match status.keypressed with
    | true -> raise Exit
    | false -> (
      let updated_buffer = tick t s prev_buffer in
      buffer_to_image s updated_buffer |> (fun b -> draw_image b 0 0);
      synchronize ();
      (* Unix.sleepf 0.05; *)
      loop (t + 1) updated_buffer
    )
  ) in loop 0 initial_buffer
