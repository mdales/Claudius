(* open Graphics *)
open Tsdl

type boot_func = Screen.t -> Framebuffer.t
type tick_func = int -> Screen.t -> Framebuffer.t -> Framebuffer.t

type bitmap_t = (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t

(* ----- *)

let (>>=) = Result.bind
let (>|=) v f = Result.map f v

let sdl_init (width : int) (height : int) (title : string) = 
  Sdl.init Sdl.Init.(video + events) >>= fun () -> 
  Sdl.create_window ~w:width ~h:height title Sdl.Window.opengl >>= fun w -> 
  Sdl.create_renderer w >|= 
  fun r -> (w, r)

let framebuffer_to_bigarray (s : Screen.t) (buffer : Framebuffer.t) (bitmap : bitmap_t) =
  let palette = Screen.palette s in
  Array.iteri (fun y row ->
    Array.iteri (fun x pixel ->
      match (Palette.index_to_rgb palette pixel) with
      | Some col -> bitmap.{x + (y * (Array.length row))} <- col
      | None -> ()
    ) row
  ) (Framebuffer.to_array buffer)

let render_texture (r : Sdl.renderer) (texture : Sdl.texture) (s : Screen.t) (bitmap : bitmap_t) =
  let width, _ = Screen.dimensions s in
  Sdl.render_clear r >>= fun () ->
  Sdl.update_texture texture None bitmap width >>= fun () ->
  Sdl.render_copy r texture >|= fun () -> 
  Sdl.render_present r

(* ----- *)

let run (title : string) (boot : boot_func option) (tick : tick_func) (s : Screen.t) =
  let width, height = Screen.dimensions s
  and scale = Screen.scale s in

  match sdl_init (width * scale) (height * scale) title with
  | Error (`Msg e) -> Sdl.log "Init error: %s" e; exit 1
  | Ok (w, r) ->
    match Sdl.create_texture r Sdl.Pixel.format_rgb888 ~w:width ~h:height Sdl.Texture.access_streaming with
    | Error (`Msg e) -> Sdl.log "texture error: %s" e; exit 1
    | Ok texture ->
      (* This is a conversion layer, but allocaing bigarrays frequently is frowned upon
         so we allocate it once here and re-use it. *)
      let bitmap = (Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout (width * height)) in

      let initial_buffer = match boot with
      | None -> Framebuffer.init (width, height) (fun _x _y -> 0)
      | Some bfunc -> bfunc s
      in

      let e = Sdl.Event.create () in
      
      let rec loop (t : int) (prev_buffer : Framebuffer.t) = (
        let updated_buffer = tick t s prev_buffer in

        framebuffer_to_bigarray s updated_buffer bitmap;

        match render_texture r texture s bitmap with 
        | Error (`Msg e) -> Sdl.log "Boot error: %s" e
        | Ok () ->
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
