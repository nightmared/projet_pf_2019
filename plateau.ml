open Decl
open Graphics

let brique_width = 25
let brique_height = 15
let brique_border = 10

let raquette_width = 35
let raquette_height = 15
(* qtté de déplacement de la raquette lors d'une entrée utilisateur *)
let raquette_offset = 10

let balle_radius = 6.


let orange = rgb 255 140 0;;
let purple = rgb 100 0 170;;

let type_briques = [
  { color = white; value = 50};
  { color = orange; value= 60};
  { color = cyan ; value = 70};
  { color = green; value = 80};
  { color = red; value = 90};
  { color = blue; value= 100};
  { color = purple; value = 110};
  { color = yellow; value= 120};
];;

let gen_brique x_idx y_idx _ height : brique =
	{
		position = (((x_idx + 1) * brique_border + (x_idx) * brique_width),
		height-((y_idx + 1) * brique_border + (y_idx-1) * brique_height));
		lifetime = Int 1 ; 
		properties = List.nth type_briques (Random.int (List.length type_briques))
	};;

(* TODO: ajouter de l'aléatoire là-dedans - c'est la raison d'être de l'utilisation
 * du type Option ici *)
let gen_terrain width height =
	(* solution entière de l'équation width =
	 * (nb_briques_par_ligne+1)*brique_border+nb_briques_par_ligne*brique_width *)
	let nb_briques_par_ligne = (width-brique_border)/(brique_border+brique_width)
	(* on réserve quelques lignes en bas pour que le jeu soit jouable *)
	in let nb_briques_par_colonne = (height-brique_border)/(brique_border+brique_height) - 6
	(* on conserve uniquement les éléments non nuls *)
	in let liste_briques = List.filter_map
		(fun x -> x)
		(List.flatten
			(List.init nb_briques_par_ligne
				(fun x_idx -> List.init nb_briques_par_colonne
					(fun y_idx -> Some(gen_brique x_idx y_idx width height))
				)
			)
		) in
		liste_briques;;


let etat_local_initial (size_win_x, size_win_y)  : local_etat =
	let terrain = gen_terrain size_win_x size_win_y
	in let balle = { 
		pos = float_of_int (size_win_x/2), (float_of_int raquette_height)+.balle_radius ; 
		direction = (4.5, 4.5)
	}
	in let raquette = { 
			position = ((size_win_x-raquette_width)/2, 0) ;
			vitesse_deplacement = (0., 0.)
	}
	in { terrain = terrain ; balle = balle ; raquette = raquette; nb_vies = 3};;