type color = Vec.vec3

let write_pixel oc ({ x = r; y = g; z = b } : color) =
    Stdlib.Printf.fprintf oc "%d %d %d\n"
    (Int.of_float (255.999 *. r))
    (Int.of_float (255.999 *. g))
    (Int.of_float (255.999 *. b));
