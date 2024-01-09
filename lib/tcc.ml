(* open Graphics *)
open Tsdl

type boot_func = Screen.t -> Framebuffer.t
type tick_func = int -> Screen.t -> Framebuffer.t -> Framebuffer.t

(* let old_expanded_row (s : Screen.t) (row : int array) : color array =
  Array.concat (List.map (fun (vl : int) : color array ->
    let col = match (Palette.index_to_rgb (Screen.palette s) vl) with
    | Some x -> x
    | None -> 0
   in
      Array.make (Screen.scale s) col
    ) (Array.to_list row))

let old_buffer_to_image (s : Screen.t) (buffer : Framebuffer.t) : image =
  let raw = Framebuffer.to_array buffer in
  let img = Array.concat (List.map (fun (row : int array) : color array array ->
    let colrow = old_expanded_row s row in
      Array.make (Screen.scale s) colrow
  ) (Array.to_list raw)) in
    make_image img


(* ----- *)

let old_run (s : Screen.t) (title : string) (boot : boot_func option) (tick : tick_func)  =
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
      old_buffer_to_image s updated_buffer |> (fun b -> draw_image b 0 0);
      synchronize ();
      (* Unix.sleepf 0.05; *)
      loop (t + 1) updated_buffer
    )
  ) in loop 0 initial_buffer *)

(* ----- *)

let (>>=) = Result.bind
let (>|=) v f = Result.map f v

let sdl_init (width : int) (height : int) (title : string) = 
  Sdl.init Sdl.Init.(video + events) >>= fun () -> 
  Sdl.create_window ~w:width ~h:height title Sdl.Window.opengl >>= fun w -> 
  Sdl.create_renderer w >|= 
  fun r -> (w, r)

let render_texture (r : Sdl.renderer) (texture : Sdl.texture) (s : Screen.t) (buffer : Framebuffer.t) =
  let width, _ = Screen.dimensions s in
  Sdl.render_clear r >>= fun () ->
  Sdl.update_texture texture None (Framebuffer.to_bigarray buffer) width >>= fun () ->
  Sdl.render_copy r texture >|= fun () -> 
  Sdl.render_present r

(* ----- *)

let run (s : Screen.t) (title : string) (boot : boot_func option) (tick : tick_func)  =
  let width, height = Screen.dimensions s
  and scale = Screen.scale s in

  match sdl_init (width * scale) (height * scale) title with
  | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
  | Ok (w, r) ->
    match Sdl.create_texture r Sdl.Pixel.format_rgb888 ~w:width ~h:height Sdl.Texture.access_streaming with
    | Error (`Msg e) -> Sdl.log "texture error: %s" e; exit 1
    | Ok texture ->

      let initial_buffer = match boot with
      | None -> Framebuffer.init (width, height) (fun _x _y -> 0)
      | Some bfunc -> bfunc s
      in

      let rec loop (t : int) (prev_buffer : Framebuffer.t) = (
        let updated_buffer = tick t s prev_buffer in
        match render_texture r texture s updated_buffer with 
        | Error (`Msg e) -> Sdl.log "Boot error: %s" e
        | Ok () ->
          let e = Sdl.Event.create () in
          match Sdl.poll_event (Some e) with
          | true -> (
            match Sdl.Event.(enum (get e typ)) with
            | `Quit -> ()
            | _ -> loop (t + 1) updated_buffer
          )
          | false -> loop (t + 1) updated_buffer
      ) in loop 0 initial_buffer;

      Sdl.destroy_texture texture;
      Sdl.destroy_renderer r;
      Sdl.destroy_window w;
      Sdl.quit ()
