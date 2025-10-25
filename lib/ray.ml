open Base
open Vec
open V.Types

type ray = { origin: point; direction: vec3 }

let ( @ ) ray step = V.(ray.origin +! (ray.direction *!. step))

type sphere_t = { origin : point; radius : float }

(* Returns point of intersection on ray *)
let check_sphere_intersection sphere (ray : ray) =
    let orig_to_sphere = V.(sphere.origin -! ray.origin) in
    let a = V.lensq ray.direction in
    let h = V.(ray.direction ^! orig_to_sphere) in
    let c = Float.O.(V.lensq orig_to_sphere - (sphere.radius ** 2.)) in 
    let discriminant = Float.O.(h ** 2. - a * c) in

    if Float.O.(discriminant < 0.) then
        None
    else
        Some Float.O.((h - Float.sqrt discriminant) / a)


module Surface = struct
    type t =
        | Sphere of { origin : point; radius : float }

    let check_hit ray surface =
        match surface with
        | Sphere sphere -> check_sphere_intersection sphere ray
end
