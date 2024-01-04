open Claudius

let fract_shader (t : int) (s : Screen.t) (x : int) (y : int) = 
  let ft = (Float.of_int t) /. 50. in
  let cx = 0.6 *. sin ft
  and cy = 1.0 *. cos ft in

  let width, height = Screen.dimensions s in
  let fxoffset = ((Float.of_int width) /. 2.)
  and fyoffset = (Float.of_int height) /. 2. in
  let fxstep = 3. /. (Float.of_int width) 
  and fystep = 2.25 /. (Float.of_int height) in
  let max_iterations = 32 in
  
  let fy = Float.of_int y in
  let py = ((fy -. fyoffset) *. fystep) in
  let fx = Float.of_int x in
  let px = ((fx -. fxoffset) *. fxstep) in

  let rec loop (a : float) (b : float) (i : int) : int option = 
    match (i == max_iterations) with
    | true -> None
    | false -> (
      let current = ((a *. a) +. (b *. b)) <= 4. in
      match current with
      | false -> Some i
      | true -> (loop 
        ((((a *. a) -. (b *. b)) +. 0.) +. cx)
        (((2. *. a *. b) +. 0.) +. cy)
        (i + 1))) in
  let col = loop px py 0 in
  match col with
  | None -> 0
  | Some col -> (col mod ((Palette.size (Screen.palette s)) - 1)) + 1

let tick t s _p =
  Framebuffer.init (Screen.dimensions s) (fract_shader t s)

let () =
  let pal = 0x000000 :: (List.rev (Palette.to_list (Palette.generate_plasma_palette 31))) in
  let screen = Screen.create 640 480 1 (Palette.of_list pal) in
  Tcc.run screen "Genuary Day 4: Pixels" None tick
