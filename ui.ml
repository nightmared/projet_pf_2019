open Graphics
open Decl

let brique_width = 25;;
let brique_height = 15;;
let brique_border = 10;;


let intersect dir distance terrain = ();;

(* TODO: ajouter de l'aléatoire là-dedans *)
let gen_terrain width height =
    (* solution entière de l'équation width =
    (nb_briques_par_ligne+1)*brique_border+nb_briques_par_ligne*brique_width *)
    let nb_briques_par_ligne = (width-brique_border)/(brique_border+brique_width)
    (* on réserve 5 lignes en bas pour que le jeu soit jouable *)
    in let nb_briques_par_colonne = (height-brique_border)/(brique_border+brique_height) - 5
    in List.flatten
        (List.init nb_briques_par_ligne
            (fun y_idx -> List.init nb_briques_par_colonne 
                (fun x_idx ->
                    Brique (
                        (float_of_int ((x_idx+1) * brique_border+x_idx*brique_width),
                        float_of_int ((y_idx+1) * brique_border+y_idx*brique_height)),
                        Some 1,
                        NilProp
                    )
                )
            )
        )

let draw_terrain terrain = ()

let run () =
    let win = open_graph ""
    in let _ = gen_terrain (size_x win) (size_y win)
    in ()