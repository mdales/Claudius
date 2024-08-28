open Claudius

type point = {
  x : float ;
  y : float ;
  z : float ;
}

let vapour_palette = "000:0000007400b86930c35e60ce5390d94ea8de48bfe356cfe164dfdf72efdd80ffdb"


let vpalette = Palette.load_tic80_palette vapour_palette

(* ----- *)

let rotate_x (p : point) (a : float) : point =
  {
    x = p.x ;
    y = (p.y *. (cos (a))) -. (p.z *. sin(a)) ;
    z = (p.y *. sin(a)) +. (p.z *. cos(a)) ;
  }

let rotate_y (p : point) (a : float) : point =
  {
    x = (p.x *. cos(a)) -. (p.z *. sin(a)) ;
    y = p.y ;
    z = (p.x *. sin(a)) +. (p.z *. cos(a)) ;
  }

let rotate_z (p : point) (a : float) : point =
  {
    x = (p.x *. cos(a)) -. (p.y *. sin(a)) ;
    y = (p.x *. sin(a)) +. (p.y *. cos(a)) ;
    z = p.z ;
  }

let point_cmp (a : point) (b : point) : int =
  if a.z == b.z then 0
  else if a.z < b.z then 1 else -1

(* ----- *)

let tick t s _fb _i =
  let fb = Framebuffer.init (Screen.dimensions s) (fun _ _ -> 0) in
  let palsize = (Palette.size (Screen.palette s) - 1) in
  let w, h = Screen.dimensions s in

  let ft = float_of_int t in
  let ft = ft /. 3. in
  let o = sin (ft /. 10.) in
  let offset = if o < 0. then ((0. -. o) *. 4.0) else 0. in
  let points : point array = Array.make (17 * 17 * 17) {x=0. ; y=0. ; z=0. } in
  for z = 0 to 16 do
    for y = 0 to 16 do
      for x = 0 to 16 do
        let p : point = {
          x = float_of_int ((x - 8)) *. (4. +. offset) ;
          y = float_of_int ((y - 8)) *. (4. +. offset) ;
          z = float_of_int ((z - 8)) *. (4. +. offset) ;
        } in
        points.(x + (y * 17) + (z * 17 * 17)) <- rotate_z(
          rotate_x(
            rotate_y p (0.02 *. ft)
          ) (0.01 *. ft)
        ) (0.005 *. ft)
      done
    done
  done;
  Array.sort point_cmp points;
  let m = 2000. +. cos(ft /. 30.) *. 600.
  and z = 10. +. sin(ft /. 1000.) *. 5.
  and d = 10. +. cos(ft /. 1000.) *. 5. in
  Array.iter (fun e ->
    let fcol = (sin(sin((e.x +. ft) /. z)) +. sin(sin((e.y +. ft) /. d)) +. sin(sin((e.z +. ft) /. z))) *. 8. in
    let col = ((int_of_float fcol) mod palsize) in
    Framebuffer.filled_circle
      ((w / 2) + int_of_float(m *. e.x /. (e.z +. 400.)))
      ((h / 2) + int_of_float(m *. e.y /. (e.z +. 400.)))
      (m /. 200.)
      ((if col < 0 then (col + palsize) else col) + 1)
      fb
  ) points;
  fb


let slide = (vpalette, None, tick)
