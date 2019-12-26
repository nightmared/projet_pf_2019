open Graphics
open Decl

let brique_width = 25;;
let brique_height = 15;;
let brique_border = 10;;


(* outils utilitaires pour le calcul de collisions *)
let minus_direction (x, y) (x_prime, y_prime) = (x-.x_prime, y-.y_prime);;
let direction_to_float (x, y) = (float_of_int x, float_of_int y);;
let cross_product (x1, y1) (x2, y2) = x1*.y2 -. y1*.x2;;

(* basé sur la méthode décrite sur https://stackoverflow.com/questions/563198/how-do-you-detect-where-two-line-segments-intersect/565282 *)
let intersect_ray p q r s =
    let rs = cross_product r s
    in let qpr = cross_product (minus_direction q p) r
    in let qps = cross_product (minus_direction q p) s
    in if rs <> 0. then
        let u = qpr/.rs
        in let t = qps/.rs
        in u >= 0. && u <= 1. && t >= 0. && t <= 1.
    (* le cas colinéaire signifie qu'il n'y a pas de collisions pour nous *)
    else false;;

let intersect_rectangle (x, y) dir rect =
    (intersect_ray (x, y) rect dir (direction_to_float (0, brique_height))
    || intersect_ray (x, y) rect dir (direction_to_float (brique_width, 0))
    || intersect_ray (x+.(float_of_int brique_width), y) rect dir (direction_to_float (0, brique_height))
    || intersect_ray (x, y+.(float_of_int brique_height)) rect dir (direction_to_float (brique_width, 0)))

let intersect pos dir terrain = 
    List.fold_left
        (fun acc (Brique(rect, _, _)) -> acc || intersect_rectangle pos dir (direction_to_float rect))
        false
        terrain;;

(* TODO: ajouter de l'aléatoire là-dedans *)
let gen_terrain width height =
    (* solution entière de l'équation width =
    (nb_briques_par_ligne+1)*brique_border+nb_briques_par_ligne*brique_width *)
    let nb_briques_par_ligne = (width-brique_border)/(brique_border+brique_width)
    (* on réserve quelques lignes en bas pour que le jeu soit jouable *)
    in let nb_briques_par_colonne = (height-brique_border)/(brique_border+brique_height) - 6
    in List.flatten
        (List.init nb_briques_par_ligne
            (fun x_idx -> List.init nb_briques_par_colonne
                (fun y_idx ->
                    Brique (
                        (((x_idx + 1) * brique_border + (x_idx) * brique_width),
                        height-((y_idx + 1) * brique_border + (y_idx-1) * brique_height)),
                        Some 1,
                        NilProp
                    )
                )
            )
        )

let draw_terrain =
    List.iter (fun (Brique ((x, y), _, _)) ->
        draw_rect x y brique_width brique_height
    )

let run () =
    let win = open_graph " 640x480"
    in let terrain = gen_terrain (size_x win) (size_y win)
    in draw_terrain terrain
