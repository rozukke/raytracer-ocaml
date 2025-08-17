module V = struct
    open Vec
    let ( +! ), ( -! ), ( *! ), ( /! ) = vadd, vsub, vmul, vdiv
    let ( +!. ), ( -!. ), ( *!. ), ( /!. ) = vadds, vsubs, vmuls, vdivs
    let norm = Vec.norm
end

