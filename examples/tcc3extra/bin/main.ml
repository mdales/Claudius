open Claudius

type point = {
  x : int ;
  y : int ;
}

type line = {
  a : point ;
  b : point ;
}

let generate_mono_palette (size : int) : int list = 
  List.init size (fun (index : int): int ->
    let fi = float_of_int index and fsize = float_of_int size in
    let ch = (cos (fi *. ((2.0 *. Float.pi) /. fsize)) *. 127.0) +. 128.0 in
    ((int_of_float ch) * 65536) + ((int_of_float ch) * 256) + (int_of_float ch)
  )


let generat_points (count : int) (t : int) (screen : Screen.t) : point list =
  let w, h = Screen.dimensions screen in
  Random.init 42;
  List.init count (fun index -> 
    {
      x = ((Random.int w) + (((index + 1) * t) / 200)) mod w ;
      y = ((Random.int h) + (((index + 1) * t) / 200)) mod h ;
    }
  )

let distance (p1 : point) (p2 : point) : int =
  int_of_float (Float.sqrt((Float.pow (float_of_int (p2.x - p1.x)) 2.) +. (Float.pow (float_of_int (p2.y - p1.y)) 2.)))

let lines_from_points (points : point list) (threshold : int) : line list = 
  List.concat (List.map ( fun (outer : point) -> 
    List.filter_map ( fun (inner : point) : line option ->
      let d = distance inner outer in
      if (d > 0) && (d <= threshold) then Option.some {
        a = inner ;
        b = outer ;
      } else Option.none
    ) points
  ) points)

(* ----- *)

let tick t s _p =
  let buffer = Framebuffer.init (Screen.dimensions s) (fun _x _y -> 0) in

  let ft = (Float.of_int t) /. 10. in
  let threshold = 80 + (int_of_float ((sin (ft /. 10.)) *. 20.)) in

  let points = generat_points 150 t s in
  let lines = lines_from_points points threshold in 
  List.iter (fun (v : line) ->
    let d = distance v.a v.b in
    let c = (((threshold - d) * ((Palette.size (Screen.palette s)) - 1)) / (threshold - 1)) in
    Framebuffer.draw_line v.a.x v.a.y v.b.x v.b.y c buffer
  ) lines;
  buffer


(* ----- *)

let () =
  Palette.of_list (generate_mono_palette 16) |>
  Screen.create 640 480 1 |>
  Base.run "TCC Day 3 Extra" None tick
