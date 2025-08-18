open Mod

let () =
    let root_of_evil : Mod.exported = {
        outer = 0;
        hidden = { _val_hidden = 0 }
    } in

    Printf.printf "%f\n" root_of_all_evil.outer;
