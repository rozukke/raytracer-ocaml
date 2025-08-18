open Base.Float.O

module V = struct

type vec3 = { x: float; y: float; z: float }

let init x y z = { x = x; y = y; z = z } [@@inline]

open struct
  (* Vector *)
  let vadd v1 v2 = { x = v1.x + v2.x; y = v1.y + v2.y; z = v1.z + v2.z} [@@inline]
  let vsub v1 v2 = { x = v1.x - v2.x; y = v1.y - v2.y; z = v1.z - v2.z} [@@inline]
  let vmul v1 v2 = { x = v1.x * v2.x; y = v1.y * v2.y; z = v1.z * v2.z} [@@inline]
  let vdiv v1 v2 = { x = v1.x / v2.x; y = v1.y / v2.y; z = v1.z / v2.z} [@@inline]

  (* Scalar *)
  let vadds v1 s = { x = v1.x + s; y = v1.y + s; z = v1.z + s} [@@inline]
  let vsubs v1 s = { x = v1.x - s; y = v1.y - s; z = v1.z - s} [@@inline]
  let vmuls v1 s = { x = v1.x * s; y = v1.y * s; z = v1.z * s} [@@inline]
  let vdivs v1 s = { x = v1.x / s; y = v1.y / s; z = v1.z / s} [@@inline]

  let dot v1 v2 = (v1.x * v2.x) + (v1.y * v2.y) + (v1.z * v2.z) [@@inline]
end

(* Operator aliases *)
let ( +! ), ( -! ), ( *! ), ( /! ), ( ^! ) = vadd, vsub, vmul, vdiv, dot
let ( +!. ), ( -!. ), ( *!. ), ( /!. ) = vadds, vsubs, vmuls, vdivs

let len v = sqrt (v.x ** 2. + v.y ** 2. + v.z ** 2.) [@@inline]
let lensq v = len v ** 2. [@@inline]
let norm v = v /!. (len v) [@@inline]

module Consts = struct
  let zero = init 0. 0. 0.
  let unit = init 1. 1. 1.
end

module Types = struct
  type color = vec3
  type point = vec3
  type nonrec vec3 = vec3
end

end
