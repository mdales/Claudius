open Claudius

let state = ref (0., 0.)


let tick _t s fb _i =
    Framebuffer.shader_inplace (fun p -> 
        if (p < 128) then p else p - 1
    ) fb;
    let _, h = Screen.dimensions (s) in
    let maxcol = ((Palette.size (Screen.palette s)) - 1) in
    let r = Random.float 1. in
    let x, y = !state in
    let xn, yn = (
        if r < 0.01 then (0.0, 0.16 *. y) else (
            if r < 0.86 then (0.85 *. x +. 0.04 *. y, -0.04 *. x +. 0.85 *. y +. 1.6) else (
                if r < 0.93 then (0.2 *. x -. 0.26 *. y, 0.23 *. x +. 0.22 *. y +. 1.6) else (
                    -0.15 *. x +. 0.28 *. y,  0.26 *. x +. 0.24 *. y +. 0.44
                )
            )
        )
    ) in
    Framebuffer.pixel_write 
        ((Int.of_float (yn *. 50.) + 80))
        ((Int.of_float (xn *. 60.)) + (h / 2))
        maxcol fb;
    state := (xn, yn);
    fb

let () = 
    Palette.of_list (List.map (fun x -> x land 0x00ff00) ( (Palette.to_list (Palette.generate_mono_palette 256)))) |>
    Screen.create 640 480 1 |>
    Base.run "Genuary 26: Grow Something" None tick
