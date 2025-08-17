open V

type point3 = Vec.vec3
type vec3 = Vec.vec3

type ray = { origin: point3; direction: vec3 }

let ( @ ) ray step = V.(ray.origin +! (ray.direction *!. step))


