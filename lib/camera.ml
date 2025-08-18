open Vec
open V.Types

open struct
type viewport = {
    _width: float;
    _height: float;
    pixel_delta_h: vec3;
    pixel_delta_v: vec3;
    topleft: point;
}
end

type sensor = {
    width: int;
    height: int;
}

type camera = {
    _viewport : viewport;
    _focal_length : float;
    _center : point;
    sensor : sensor;
}

let create image_width focal_length (center : point) : camera =
    let aspect_ratio = 16. /. 9. in
    let image_height = Int.of_float (Float.of_int image_width /. aspect_ratio) in

    let viewport_height = 2.0 in
    let viewport_width = viewport_height *. (Float.of_int image_width /. Float.of_int image_height) in
    let viewport_hvec = V.init viewport_width 0. 0. in
    let viewport_vvec = V.init 0. (Float.neg viewport_height) 0. in
    let viewport_topleft = V.(center -! (V.init 0. 0. focal_length) -! viewport_hvec /!. 2. -! viewport_vvec /!. 2.) in
    let pix_delta_h = V.(viewport_hvec /!. Float.of_int image_width) in
    let pix_delta_v = V.(viewport_vvec /!. Float.of_int image_height) in

    let viewport = {
        _height = viewport_height;
        _width = viewport_width;
        pixel_delta_h = pix_delta_h;
        pixel_delta_v = pix_delta_v;
        topleft = V.(viewport_topleft +! (pix_delta_h +! pix_delta_v) *!. 0.5)
    } in

    {
        _viewport = viewport;
        _focal_length = focal_length;
        _center = center;
        sensor = { width = image_width; height = image_height };
    }

let get_pixel_ray camera row col : Ray.ray =
    let pixel_loc = V.(camera._viewport.topleft +! (camera._viewport.pixel_delta_h *!. Float.of_int col) +! (camera._viewport.pixel_delta_v *!. Float.of_int row)) in
    {
        origin = camera._center;
        direction = V.(pixel_loc -! camera._center)
    }


let write_pixel oc ({ x = r; y = g; z = b } : color) =
    Stdlib.Printf.fprintf oc "%d %d %d\n"
    (Int.of_float (255.999 *. r))
    (Int.of_float (255.999 *. g))
    (Int.of_float (255.999 *. b));

