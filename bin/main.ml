open Base
open Raytracer

(* Using the operator fuckery in this language to its fullest:
    Module V defines:
    - Vector -> Vector operations with (operator) + (!),
    as in: vec +! vec -> vec
    - Vector -> Scalar operations with (operator) + (!.),
    as in: vec *!. float -> vec
*)

let image_file = "out.ppm"

let aspect_ratio = 16. /. 9.
let image_width = 1280
let image_height = Int.of_float (Float.of_int image_width /. aspect_ratio)

(* Viewport *)
let viewport_height = 2.0
let viewport_width = viewport_height *. (Float.of_int image_width /. Float.of_int image_height)

let focal_length = 1.
let camera_center : vec3 = { x = 0.; y = 0.; z = 0. }

(* Vector right across horizontal viewport direction *)
let viewport_u : vec3 = { x = viewport_width; y = 0.; z = 0. }
(* Vector down across vertical viewport direction *)
let viewport_v : vec3 = { x = 0.; y = -.viewport_height; z = 0. }

(* Vector of a single pixel length *)
let pixel_delta_u = V.(viewport_u /!. Float.of_int image_width)
let pixel_delta_v = V.(viewport_v /!. Float.of_int image_height)

(* Viewport origin *)
let viewport_topleft = V.(camera_center -! { x = 0.; y = 0.; z = focal_length } -! viewport_u /!. 2. -! viewport_v /!. 2.)
let topleft_pixel_center = V.(viewport_topleft +! (pixel_delta_u +! pixel_delta_v) *!. 0.5)


let ray_color (ray : vec3) : color =
    let lightblue : color = { x = 0.5; y = 0.7; z = 1.0 } in
    let white : color = { x = 1.0; y = 1.0; z = 1.0 } in
    let norm_dir = V.norm ray in
    (* Height of ray between top and bottom of frame as proportion of 1.0 *)
    let alpha = (norm_dir.y +. 1.) *. 0.5 in
    (* Lerp between white and blue depending on y height *)
    V.(
        white *!. (1.0 -. alpha) +! lightblue *!. alpha
    )

let () =
    let open Stdlib.Printf in

    let oc = Stdio.Out_channel.create image_file in

    (* Header *)
    fprintf oc "P3\n%d %d\n255\n" image_width image_height;

    (* Pixel loop *)
    printf "Progress: ___%%";
    for row = 0 to image_height - 1 do

        (* Interactive progress counter *)
        let prcnt_progress = ((row + 1) * 100) / (image_height + 1) in
        printf "\b\b\b\b";
        printf "%3d%%%!" (prcnt_progress + 1);

        (* Draw row of pixels *)
        for col = 0 to image_width - 1 do
            let pixel_center = V.(topleft_pixel_center +! (pixel_delta_u *!. Float.of_int col) +! (pixel_delta_v *!. Float.of_int row)) in
            let out_color = ray_color V.(pixel_center -! camera_center) in
            write_pixel oc out_color;
        done
    done;
    printf "\n";
