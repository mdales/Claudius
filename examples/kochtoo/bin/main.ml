open Claudius

type turtle = {
  angle : float;
  path : (float * float * bool) list;
  mark : bool ;
}

let tic80_palette = "000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57"

(* ----- *)

let left (a : float) (t : turtle) : turtle = 
  { t with angle = t.angle -. ((a /. 360.) *. 2. *. Float.pi)}

let right (a : float) (t : turtle) : turtle = 
  { t with angle = t.angle +. ((a /. 360.) *. 2. *. Float.pi)}

let forward (dist : float) (t : turtle) : turtle = 
  let x, y, _ = match t.path with
  | [] -> 0., 0., false
  | hd :: _ -> hd
  in
  let newpos = (
    x +. ((sin t.angle) *. dist),
    y +. ((cos t.angle) *. dist),
    t.mark
  ) in
  { t with path = newpos :: t.path }

let penup (t : turtle) : turtle = 
  {t with mark = false}

let pendown (t : turtle) : turtle = 
  {t with mark = true}

(* ----- *)

let rec kock (length : float) (level : int) (t : turtle) : turtle  =
  match level with
  | 0 -> forward length t
  | _ -> (
    let l = length /. 3. in
      kock l (level - 1) t |>
      left 60. |>
      kock l (level - 1) |>
      right 120. |>
      kock l (level - 1) |>
      left 60. |>
      kock l (level - 1)
  )

let star ~(length : float) ~(level : int) ~(turtle : turtle) : turtle = 
  let moved_t = penup turtle |>
  left (360. /. 3.) |>
  forward length |>
  right (360. /. 3.) |>
  pendown in

  let rec loop (index : int) (t : turtle) : turtle =
    match index with
    | 0 -> t
    | _ -> 
      kock length level t |>
      right 60. |>
      loop (index - 1) 
    in
      loop 6 moved_t

(* ----- *)

let tick (t : int) (screen : Screen.t) (_prev : Framebuffer.t) : Framebuffer.t =
  let width, height = Screen.dimensions screen in
  
  let stars = List.init 3 (fun i -> 
    let col = (10 + i) in
    let turtle = {
      angle = 0. ;
      path = [ ((Float.of_int (width / 2)), (Float.of_int (height / 2)), false) ] ;
      mark = false ;
    } in
  
    let ft = (Float.of_int t) /. 10. and fi = Float.of_int (2 - i) in
    let length = (Float.rem (2. *. (ft +. (fi *. 150.))) 450.) in
    let star = (
      let t = left (fi +. (3. *. ft)) turtle in
      star 
        ~length:length 
        ~level:(abs (Int.of_float (5. *. sin (ft /. 10.))))
        ~turtle:t
    ) in
    let steps = List.rev star.path in
    (length, steps, col)
  ) |>
  List.sort (fun a b ->
    let lena, _, _ = a 
    and lenb, _, _ = b in
    Int.of_float(lenb -. lena)
  ) in

  let _, _, bg = List.hd (List.rev stars) in
  let buffer = Framebuffer.init (width, height) (fun _x _y -> bg) in

  List.iter (fun s ->
    let _, steps, col = s in
    Framebuffer.filled_polygon (List.filter_map (fun step ->
      let x, y, pen = step in
      match pen with
      | true -> Some ((Int.of_float x), (Int.of_float y))
      | false -> None
    ) steps) col buffer
  ) stars;
  buffer
  
(* ----- *)

let () =
  Palette.load_tic80_palette tic80_palette |>
  Screen.create 640 480 1 |>
  Base.run "TCC Day 7 Extra Extra" None tick
