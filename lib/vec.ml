open Base

type vec3 = { x: float; y: float; z: float }

let vadd v1 v2 = Float.O.({ x = v1.x + v2.x; y = v1.y + v2.y; z = v1.z + v2.z})
let vsub v1 v2 = Float.O.({ x = v1.x - v2.x; y = v1.y - v2.y; z = v1.z - v2.z})
let vmul v1 v2 = Float.O.({ x = v1.x * v2.x; y = v1.y * v2.y; z = v1.z * v2.z})
let vdiv v1 v2 = Float.O.({ x = v1.x / v2.x; y = v1.y / v2.y; z = v1.z / v2.z})

let vadds v1 s = Float.O.({ x = v1.x + s; y = v1.y + s; z = v1.z + s})
let vsubs v1 s = Float.O.({ x = v1.x - s; y = v1.y - s; z = v1.z - s})
let vmuls v1 s = Float.O.({ x = v1.x * s; y = v1.y * s; z = v1.z * s})
let vdivs v1 s = Float.O.({ x = v1.x / s; y = v1.y / s; z = v1.z / s})

let length v = Float.O.(Float.sqrt (v.x ** 2. + v.y ** 2. + v.z ** 2.))
let norm v = vdivs v (length v)
