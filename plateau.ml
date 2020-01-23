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
let balle_initiale_speed = 300.


let orange = rgb 255 140 0;;
let purple = rgb 100 0 170;;

(* Nombre de bonus différents à prendre en compte *)
let nb_type_bonus = 5;;

(* durabilité maximale des briques *)
let vie_max_brique = 3;;

type type_brique = {color : color; value : int};;

(* Liste des différents type de briques pas lifetime croissant *)
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

(* Génére une brique à partir de sa position 
   Son lifetime et le bonus qu'elle contient sont choisis aléatoirement *)
let gen_brique x_idx y_idx _ height : brique =
  let x_idx = float_of_int x_idx in 
  let y_idx = float_of_int y_idx in
  let lifetime = Random.int (vie_max_brique+1) in
  let est_bonus =  Random.float 1. < 0.05  in
  let bonus = if est_bonus then (Random.int nb_type_bonus) + 1 else 0 in
  let type_brique = List.nth type_briques lifetime in {
    position = (((x_idx +. 1.) *. brique_border +. (x_idx) *. brique_width),
    height-.((y_idx +. 1.) *. brique_border +. (y_idx-.1.) *. brique_height));
        properties = { color = type_brique.color; value = type_brique.value; bonus = if est_bonus && lifetime != 0 then
                                                                                      match bonus with
                                                                                      | 1 -> Some OneMoreLife
                                                                                      | 2 -> Some SpeedUp
                                                                                      | 3 -> Some SpeedDown
                                                                                      | 4 -> Some SizeUp
                                                                                      | 5 -> Some SizeDown
                                                                                      | _ -> None
                                                                                      else None };
    lifetime = if lifetime = 0 then Infinity else Int (lifetime)
  };;

(* Génère l'état initiale du terrain à partir de ses dimensions *)
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
          (fun y_idx -> if Random.int 5 <= 1 then Some(gen_brique x_idx (y_idx + 1) width height) else None)
        )
      )
    ) in
    liste_briques

(* Initialisation de l'état local de la partie 
   rappel : état local = terrain + balle + raquette + nb_vies *)
let etat_local_initial (size_win_x, size_win_y)  : etat_local =
  let terrain = gen_terrain size_win_x size_win_y
  in let balle = { 
    pos = size_win_x/.2., raquette_height +. balle_radius ; 
    direction = (balle_initiale_speed/.ffrequence, balle_initiale_speed/.ffrequence)
  }
  in let raquette = { 
      position = (size_win_x -.raquette_width) /. 2., 0. ;
      vitesse_deplacement = (0., 0.);
      width = raquette_width
  }
  in { terrain = terrain ; balle = balle ; raquette = raquette; nb_vies = 3}

(* Retourne les valeurs relatives à la position et aux dimensions d'une brique *)
let brique_to_aabb (brique : brique) = { point= brique.position; width =brique_width; height =brique_height};;

(* Retourne la position du centre d'un rectangle (brique ou raquette) *)
let centre_rectangle (r : aabb) = 
  r.point +$ (1./.2.) *: (r.width, r.height)
