open Graphics

open Decl

let brique_width = 25
let brique_height = 15
let brique_border = 10

let raquette_width = 20
let raquette_height = 15
(* qtté de déplacement de la raquette lors d'une entrée utilisateur *)
let raquette_offset = 30

let balle_radius = 6


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
	(* on vérifie les intersections avec les quatres côtés du rectangle *)
	(intersect_ray (x, y) rect dir (direction_to_float (0, brique_height))
	|| intersect_ray (x, y) rect dir (direction_to_float (brique_width, 0))
	|| intersect_ray (x+.(float_of_int brique_width), y) rect dir (direction_to_float (0, brique_height))
	|| intersect_ray (x, y+.(float_of_int brique_height)) rect dir (direction_to_float (brique_width, 0)))

let intersect pos dir (Terrain terrain) =
	List.fold_left
		(fun acc v -> let (Brique(rect, _, _)) = v in if intersect_rectangle pos dir (direction_to_float rect)
			then v::acc else acc
		)
		[]
		terrain;;

let gen_brique x_idx y_idx _ height =
	Brique (
		(((x_idx + 1) * brique_border + (x_idx) * brique_width),
		height-((y_idx + 1) * brique_border + (y_idx-1) * brique_height)),
		Some 1,
		NilProp
	)

(* TODO: ajouter de l'aléatoire là-dedans - c'est la raison d'être de l'utilisation
 * du type Option ici *)
let gen_terrain width height =
	(* solution entière de l'équation width =
	 * (nb_briques_par_ligne+1)*brique_border+nb_briques_par_ligne*brique_width *)
	let nb_briques_par_ligne = (width-brique_border)/(brique_border+brique_width)
	(* on réserve quelques lignes en bas pour que le jeu soit jouable *)
	in let nb_briques_par_colonne = (height-brique_border)/(brique_border+brique_height) - 6
	(* on conserve uniquement les éléments non nuls *)
	in List.filter_map
		(fun x -> x)
		(List.flatten
			(List.init nb_briques_par_ligne
				(fun x_idx -> List.init nb_briques_par_colonne
					(fun y_idx -> Some(gen_brique x_idx y_idx width height))
				)
			)
		)

let dessiner_terrain (Terrain liste_blocs) =
	List.iter (fun (Brique ((x, y), _, _)) ->
		draw_rect x y brique_width brique_height
	) liste_blocs

let dessiner_balle (Balle ((x, y), dir)) = draw_circle x y balle_radius

let dessiner_raquette (Raquette (x, y)) = draw_rect x y raquette_width raquette_height

(* déplace la raquette si possible *)
let deplacer_raquette (Raquette (x, y)) win_size gauche =
	if gauche then
		(if x-raquette_offset < 0 then (Raquette (0, y)) else (Raquette (x-raquette_offset, y)))
	else
		(if x+raquette_offset >= win_size then (Raquette (win_size-raquette_width, y)) else (Raquette (x+raquette_offset, y)))

(* modifie l'état en fonction de l'entrée clavier *)
let gerer_entree_clavier (State (LocalState (terrain, balle, raquette), GlobalState (win_size_x, win_size_y))) key =
	begin
		let raquette =
			(if key = 'd' then
				deplacer_raquette raquette win_size_x false
			else
				if key = 'q' then
					deplacer_raquette raquette win_size_x true
				else
					raquette)
		in
			(State (LocalState (terrain, balle, raquette), GlobalState (win_size_x, win_size_y)))
	end

(* Boucle évènementielle pour dessiner la raquette *)
let main_loop () =
	try
		begin
			(* pour empécher les artifacts graphiques avec le double buffering *)
			auto_synchronize false;
			while true do
				let st = wait_next_event [ Key_pressed; Poll ]
				in if st.keypressed then
					(* nécessaire pour que l'évènement ne soit pas retourné à chaque poll(),
					 * il faut vider le pool d'évènements *)
					let st = wait_next_event [ Key_pressed ]
					in let state = GreenThreadsState.get ()
					in let new_state = gerer_entree_clavier state st.key
					in GreenThreadsState.send new_state;
				GreenThreadsState.yield ();
			done;
			GreenThreadsState.exit ()
		end
	with Exit -> GreenThreadsState.exit ()

(* Détecte les collisions et supprime les blocs détruits *)
let detecter_collisions () =
	begin
		while true do
			let State (LocalState (terrain, balle, raquette), _) = GreenThreadsState.get ()
			in begin
				()
			end;
			GreenThreadsState.yield ();
		done;
		GreenThreadsState.exit ()
	end

(* Dessine les différents objets à l'écran *)
let dessiner () =
	begin
		while true do
			let State (LocalState (terrain, balle, raquette), _) = GreenThreadsState.get ()
			in begin
				clear_graph ();
				dessiner_terrain terrain;
				dessiner_balle balle;
				dessiner_raquette raquette;
				synchronize ();
			end;
			GreenThreadsState.yield ();
		done;
		GreenThreadsState.exit ()
	end

(* 'main' du programme, crée un état initial et lance l'ordonnanceur *)
let run () =
	let win = open_graph " 640x480"
	in let size_win_x = size_x win
	in let size_win_y = size_y win
	in let terrain = gen_terrain size_win_x size_win_y
	in let balle = Balle ((size_win_x/2, raquette_height+balle_radius), (0.15, 0.15))
	in let raquette = Raquette ((size_win_x-raquette_width)/2, 0)
	in let etat_initial = (State (LocalState (Terrain terrain, balle, raquette), GlobalState (size_win_x, size_win_y)))
	in GreenThreadsState.scheduler [main_loop; detecter_collisions; dessiner] etat_initial
