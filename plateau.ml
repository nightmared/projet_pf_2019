open Decl
open Graphics


let frequence = 10000
let ffrequence = float_of_int frequence
let brique_width = 25.
let brique_height = 15.
let brique_border = 10.

let raquette_width = 35.
let raquette_height = 15.
(* qtté de déplacement de la raquette lors d'une entrée utilisateur *)
let raquette_offset = 10.

let balle_radius = 6.120


let orange = rgb 255 140 0;;
let purple = rgb 100 0 170;;

type type_brique = {color : color; value : int};;

let type_briques = [
  { color = white; value = 50;};
  { color = orange; value= 60};
  { color = cyan ; value = 70};
  { color = green; value = 80};
  { color = red; value = 90};
  { color = blue; value= 100};
  { color = purple; value = 110};
  { color = yellow; value= 120};
]

let gen_brique x_idx y_idx _ height : brique =
  let x_idx = float_of_int x_idx in 
  let y_idx = float_of_int y_idx in
  let lifetime = Random.int (List.length type_briques) in
  let est_bonus =  Random.float 1. < 0.05  in
  let type_brique = List.nth type_briques lifetime in {
    position = (((x_idx +. 1.) *. brique_border +. (x_idx) *. brique_width),
    height-.((y_idx +. 1.) *. brique_border +. (y_idx-.1.) *. brique_height));
    lifetime = if lifetime = 0 then Infinity else Int (lifetime/3);
    properties = { color = type_brique.color; value = type_brique.value; bonus = if est_bonus then Some OneMoreLife else None}
  };;

let gen_terrain width height =
  (* solution entière de l'équation width =
   * (nb_briques_par_ligne+1)*brique_border+nb_briques_par_ligne*brique_width *)
  let nb_briques_par_ligne = int_of_float ((width-.brique_border)/.(brique_border+.brique_width))
  (* on réserve quelques lignes en bas pour que le jeu soit jouable *)
  in let nb_briques_par_colonne = int_of_float ((height-.brique_border)/.(brique_border+.brique_height) -. 6.)
  (* on conserve uniquement les éléments non nuls *)
  in let liste_briques = List.filter_map
    (fun x -> x)
    (List.flatten
      (List.init nb_briques_par_ligne
        (fun x_idx -> List.init nb_briques_par_colonne
          (fun y_idx -> if Random.int 5 <= 1 then Some(gen_brique x_idx y_idx width height) else None)
        )
      )
    ) in
    liste_briques


let etat_local_initial (size_win_x, size_win_y)  : etat_local =
  let terrain = gen_terrain size_win_x size_win_y
  in let balle = { 
    pos = size_win_x/.2., raquette_height +. balle_radius ; 
    direction = (150./.ffrequence, 150./.ffrequence)
  }
  in let raquette = { 
      position = (size_win_x -.raquette_width) /. 2., 0. ;
      vitesse_deplacement = (0., 0.)
  }
  in { terrain = terrain ; balle = balle ; raquette = raquette; nb_vies = 3}

let brique_to_aabb (brique : brique) = { point= brique.position; width =brique_width; height =brique_height};;
let centre_rectangle (r : aabb) = 
  r.point +$ (1./.2.) *: (r.width, r.height)