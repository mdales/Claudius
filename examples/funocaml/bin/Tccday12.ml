open Claudius

type point = {
  x : float ;
  y : float ;
  z : float ;
  c : int ;
}

let _tic80_palette = "000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57"
let havrekaka_palette = "000:ffffff6df7c111adc1606c813934571e88755bb361a1e55af7e476f99252cb4d686a3771c92464f48cb6f7b69e9b9c82"
let _vapour_palette = "000:7400b86930c35e60ce5390d94ea8de48bfe356cfe164dfdf72efdd80ffdb"
(* let vapour_palette = "000:7400b86930c35e60ce5390d94ea8de48bfe356cfe164dfdf72efdd80ffdb7400b86930c35e60ce5390d94ea8de48bfe3" *)


(* ----- *)

(* let rotate_x (p : point) (a : float) : point =
  {
    x = p.x ;
    y = (p.y *. (cos (a))) -. (p.z *. sin(a)) ;
    z = (p.y *. sin(a)) +. (p.z *. cos(a)) ;
  }*)

let rotate_y (p : point) (a : float) : point =
  {
    x = (p.x *. cos(a)) -. (p.z *. sin(a)) ;
    y = p.y ;
    z = (p.x *. sin(a)) +. (p.z *. cos(a)) ;
    c = p.c ;
  }

(*let rotate_z (p : point) (a : float) : point =
  {
    x = (p.x *. cos(a)) -. (p.y *. sin(a)) ;
    y = (p.x *. sin(a)) +. (p.y *. cos(a)) ;
    z = p.z ;
  } *)

let point_cmp (a : point) (b : point) : int =
  if a.z == b.z then 0
  else if a.z < b.z then 1 else -1

(* ----- *)

let rec sum x =
  if (x == 0) then 0 else x + sum (x - 1)

let tick t s _p _i =
  let w, h = Screen.dimensions s in
  (* set_palette_color screen.palette (8 + ((t  / 20) mod 8)); *)
  let fb = Framebuffer.init (w, h) (fun _ _ -> 2) in
  Random.init 42;


  let ft = float_of_int t in
  let tiers = 24 and snow_count = 100 in
  let points : point array = Array.make ((sum tiers) + snow_count) {x=10000. ; y=110. ; z=1000. ; c=15 } in
  for i = 0 to  (tiers - 1) do
    for j = 0 to i do
      let fi = float_of_int (i + 1) and fj = float_of_int (j + 1) in
      let p : point = {
        x = (fi -. 1.0) *. 10. *. (sin ((fj /. fi) *. 2. *. Float.pi));
        y = 350. -. ((fi +. 1.) *. 20.) ;
        z = (fi -. 1.0) *. 10. *. (cos ((fj /. fi) *. 2. *. Float.pi));
        c = (tiers - i + (t / 20)) mod (Palette.size (Screen.palette s)) ;
      } in
      points.(snow_count + j + (sum i)) <- rotate_y p (0.005 *. ft)
    done
  done;

  for i = 0 to (snow_count - 1) do
    let volume = 600 in
    let fall = ((((Random.int volume) - (t * 2)) mod volume)) in
    let fall2 = if fall < 0 then fall + volume else fall in
    let fall3 = fall2 - (volume / 2) in
    let p : point = {
      x = float_of_int ((Random.int volume) - (volume / 2)) ;
      y = float_of_int fall3 ;
      z = float_of_int ((Random.int volume) - (volume / 2)) ;
      c = 0 ;
    } in
    points.(i) <- rotate_y p (0.003 *. ft)
  done;

  Array.sort point_cmp points;

  let m = 200. in
  Array.iter (fun e ->
    Framebuffer.filled_circle
      ((w / 2) + int_of_float (m *. e.x /. (e.z +. 400.)))
      ((h / 2) - int_of_float (m *. e.y /. (e.z +. 400.)))
      (((200. /. ((e.z +. 500.) /. 10.)))) e.c fb
  ) points;
  fb


let palette = Palette.load_tic80_palette havrekaka_palette

let slide = (palette, None, tick)
