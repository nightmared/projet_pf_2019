open Graphics

let brique_width = 25;;
let brique_height = 15;;


let intersect dir distance terrain = ();;
let gen_terrain width height = let _ = width + height in ();;

let run () =
    let win = open_graph ""
    in gen_terrain (size_x win) (size_y win)