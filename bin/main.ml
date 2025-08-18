open Base
open Raytracer

open V.Types

(* Using the operator fuckery in this language to its fullest:
    Module V defines:
    - Vector -> Vector operations with (operator) + (!),
    as in: vec +! vec -> vec
    - Vector -> Scalar operations with (operator) + (!.),
    as in: vec *!. float -> vec
*)

let image_file = "out.ppm"


(* Returns distance of intersection *)
let ray_hit_sphere sphere_center radius (ray : ray) =
    let orig_to_sphere = V.(sphere_center -! ray.origin) in
    let a = V.lensq ray.direction in
    let h = V.(ray.direction ^! orig_to_sphere) in
    let c = Float.O.(V.lensq orig_to_sphere - (radius ** 2.)) in 
    let discriminant = Float.O.(h ** 2. - a * c) in

    if Float.O.(discriminant < 0.) then
        -1.
    else
        Float.O.((h - Float.sqrt discriminant) / a)


let ray_color (ray : ray) : color =
    (* Check collision *)
    let t = ray_hit_sphere {x=0.;y=0.;z= -1.} 0.5 ray in
    if Float.O.(t > 0.) then
        let n = V.(norm ((ray @ t) -! { x = 0.; y = 0.; z = -1.})) in
        V.((n +!. 1.) *!. 0.5) 
    else
    (* Render background *)
    let lightblue : color = { x = 0.5; y = 0.7; z = 1.0 } in
    let white : color = { x = 1.0; y = 1.0; z = 1.0 } in
    let norm_dir = V.norm ray.direction in
    (* Height of ray between top and bottom of frame as proportion of 1.0 *)
    let alpha = (norm_dir.y +. 1.) *. 0.5 in
    (* Lerp between white and blue depending on y height *)
    V.(
        white *!. (1.0 -. alpha) +! lightblue *!. alpha
    )


let () =
    let open Stdlib.Printf in

    let oc = Stdio.Out_channel.create image_file in
    let camera = Camera.create 1280 1. (V.Consts.zero) in

    (* Header *)
    fprintf oc "P3\n%d %d\n255\n" camera.sensor.width camera.sensor.height;

    (* Pixel loop *)
    printf "Progress: ___%%";
    for row = 0 to camera.sensor.height - 1 do

        (* Interactive progress counter *)
        let prcnt_progress = ((row + 1) * 100) / (camera.sensor.height + 1) in
        printf "\b\b\b\b";
        printf "%3d%%%!" (prcnt_progress + 1);

        (* Draw row of pixels *)
        for col = 0 to camera.sensor.width - 1 do
            let ray = Camera.get_pixel_ray camera row col in
            let out_color = ray_color ray in
            Camera.write_pixel oc out_color;
        done
    done;
    printf "\n";
