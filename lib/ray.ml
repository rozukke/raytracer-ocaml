open Vec
open V.Types

type ray = { origin: point; direction: vec3 }

let ( @ ) ray step = V.(ray.origin +! (ray.direction *!. step))


