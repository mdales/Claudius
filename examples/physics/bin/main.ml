open Claudius
open Hooke

let grid_width = 200
let grid_depth = 76

type item = {
  spring : Spring.t ;
  snapshot : Spring.snapshot ;
  target : float ;
}

let init_spheres (_t : int) = 
  Array.init grid_depth (fun _z -> 
    Array.init grid_width (fun _x ->
      {
        spring = Spring.make ~delta_time:(30. /. 5_000.) ~angular_freq:7. ~damping_ratio:0.15 ;
        snapshot = { position = 14. ; velocity = 0. } ;
        target = 0. ;
      }
    )
  )

let spheres = init_spheres 42

let calc_y fx fz ft = 
  let prey = ((sin ((sin (ft /. 2.)) +. fx /. 50.) *. 3.) +. (sin((fz /. 50.) -. ft)) *. 3.) in
  if prey > 0. then prey *. 4. else prey

let reset_spheres (t : int) (spheres : item array array) =
  let ft = (Float.of_int t) *. 0.02 in
  Array.iteri (fun z row ->
    let fz = (Float.of_int z) *. 8. in
    Array.mapi_inplace (fun x _ ->
      let fx = ((Float.of_int (x - (grid_width / 2))) *. 3.) in
      let y1 = calc_y fx fz ft 
      and y2 = calc_y fx fz (ft +. 20.) in
      {
        spring = Spring.make ~delta_time:(30. /. 5_000.) ~angular_freq:7. ~damping_ratio:0.15;
        snapshot = { position = (y1 *. 2.) +. 5. ; velocity = 0. };
        target = (y2 *. 2.) +. 5.;
      }
    ) row
  ) spheres

let update_spheres (spheres : item array array) =
  Array.iter (fun row ->
    Array.map_inplace (fun item ->
      let target_pos = item.target in
      let snapshot = Spring.update item.spring item.snapshot ~target_pos in
      { item with snapshot }
    ) row
  ) spheres

let tick (t : int) (screen : Screen.t) (_prev : Framebuffer.t) : Framebuffer.t =
  Random.init 42;

  if (t mod 1000) == 0 then (reset_spheres t spheres);

  let width, height = Screen.dimensions screen in
  let buffer = Framebuffer.init (width, height) (fun _x _y -> 0) in

  for z = 0 to (grid_depth - 1) do
    let fz = (Float.of_int z) *. 8. in
    for x = 0 to (grid_width - 1) do 
      let y = spheres.(z).(x).snapshot.position in


      let fx = ((Float.of_int (x - (grid_width / 2))) *. 3.) in
      let px = (width / 2) + Int.of_float (fx /. (fz *. 1.8 -. 1200.) *. 1200.) 
      and py = (Int.of_float ((y -. 40.) /. (fz *. 1.8 -. 1200.) *. 1200.)) + 100
      and col = ((Int.of_float y) mod 15) in
      (* let dither = if (Float.rem y 1.) > (Random.float 1.) then 1 else 0 in *)
      let dot = (20. /. (78. -. (fz /. 8.))) in

     Framebuffer.filled_circle px py dot (1 + (if col < 0 then col + 15 else col)) buffer
    done
  done;
  update_spheres spheres;
  buffer

(* ----- *)

let () =
  Palette.of_list (0x666666 :: (Palette.to_list (Palette.generate_plasma_palette 15))) |>
  Screen.create 640 480 1 |>
  Base.run "Genuary Day 15: Use a Physics Library" None tick