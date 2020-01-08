open Graphics

open Decl

let brique_width = 25
let brique_height = 15
let brique_border = 10

let raquette_width = 20
let raquette_height = 15
(* qtté de déplacement de la raquette lors d'une entrée utilisateur *)
let raquette_offset = 10

let balle_radius = 6.

(* retourne 'vrai' si le point est dans le rectangle *)
let est_dans_rectangle (posx, posy) (rect_x, rect_y) (rect_width, rect_height) =
	posx >= rect_x && posx <= rect_x + rect_width && posy >= rect_y && posy <= rect_y + rect_height

let collision_rectangle_haut_droite (Balle (pos, dir)) rect rect_size = est_dans_rectangle (tuple_to_int (add_tuple pos (add_tuple dir (balle_radius, balle_radius)))) rect rect_size
let collision_rectangle_haut_gauche (Balle (pos, dir)) rect rect_size = est_dans_rectangle (tuple_to_int (add_tuple pos (add_tuple dir (-.balle_radius, balle_radius)))) rect rect_size
let collision_rectangle_bas_droite (Balle (pos, dir)) rect rect_size = est_dans_rectangle (tuple_to_int (add_tuple pos (add_tuple dir (balle_radius, -.balle_radius)))) rect rect_size
let collision_rectangle_bas_gauche (Balle (pos, dir)) rect rect_size = est_dans_rectangle (tuple_to_int (add_tuple pos (add_tuple dir (-.balle_radius, -.balle_radius)))) rect rect_size

(* On vérifie les collisions de la manière suivante (la version précédente faisait
 * du 'lancer de rayon' pour détecter les collisions, mais ce n'est pas très pratique
 * puisque notre balle ne se réduit pas à un seul point *)
let collision_rectangle balle rect rect_size =
	(* on vérifie les collisions avec les quatres extrêmes de la 'bounding box' du cercle *)
	(collision_rectangle_bas_gauche balle rect rect_size
	|| collision_rectangle_bas_droite balle rect rect_size
	|| collision_rectangle_haut_gauche balle rect rect_size
	|| collision_rectangle_haut_droite balle rect rect_size)



let collision balle (Terrain terrain) =
	List.fold_left
		(fun acc v -> let (Brique(rect, _, _)) = v in if collision_rectangle balle rect (brique_width, brique_height)
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

(* supprime une liste de blocs du terrain *)
let supprimer_blocs (Terrain terrain) l =
	Terrain
		(List.fold_left (fun acc e -> List.filter_map (fun x -> if x = e then None else Some(x)) acc) terrain l)


let dessiner_terrain (Terrain liste_blocs) =
	List.iter (fun (Brique ((x, y), _, _)) ->
		draw_rect x y brique_width brique_height
	) liste_blocs

let dessiner_balle (Balle ((x, y), dir)) = draw_circle (int_of_float x) (int_of_float y) (int_of_float balle_radius)

let dessiner_raquette (Raquette (x, y)) = draw_rect x y raquette_width raquette_height

let avancer_balle (Balle ((x, y), (dx, dy))) (GlobalState (win_size_x, win_size_y)) =
	let (x, dx) = (if x+.dx > (float_of_int win_size_x)-.balle_radius then
		((float_of_int win_size_x)-.balle_radius, -.dx)
	else if (int_of_float (x+.dx)) < 0 then
		(balle_radius, -.dx)
	else
		(x+.dx, dx))
	in let (y, dy) = (if y+.dy > (float_of_int win_size_y)-.balle_radius then
		((float_of_int win_size_y)-.balle_radius, -.dy)
	else
		(y+.dy, dy))
	in (Balle ((x, y), (dx, dy)))

(* déplace la raquette si possible *)
let deplacer_raquette (Raquette (x, y)) win_size gauche =
	if gauche then
		(if x-raquette_offset < 0 then (Raquette (0, y)) else (Raquette (x-raquette_offset, y)))
	else
		(if x+raquette_offset >= win_size then (Raquette (win_size-raquette_width, y)) else (Raquette (x+raquette_offset, y)))

(* modifie l'état en fonction des mouvements de la souris *)
let gerer_entree_souris (State (LocalState (terrain, balle, raquette), GlobalState (win_size_x, win_size_y))) x =
	begin
		let Raquette ((posx, _)) = raquette
		in let raquette =
			(if x > posx+raquette_width then
				deplacer_raquette raquette win_size_x false
			else if x < posx then
				deplacer_raquette raquette win_size_x true
				else raquette)
		in
			(State (LocalState (terrain, balle, raquette), GlobalState (win_size_x, win_size_y)))
	end

(* Boucle évènementielle pour dessiner la raquette *)
let boucle_evenementielle () =
	try
		begin
			(* pour empécher les artifacts graphiques avec le double buffering *)
			auto_synchronize false;
			while true do
				let st = wait_next_event [ Poll ]
				in let etat = GreenThreadsState.get ()
				in let new_etat = gerer_entree_souris etat st.mouse_x
				in GreenThreadsState.send new_etat;
				GreenThreadsState.yield ();
			done;
			GreenThreadsState.exit ()
		end
	with Exit -> GreenThreadsState.exit ()

(* détecte une collision entre la balle et la raquette *)
let detecter_collision_raquette balle (Raquette raquette_position) g_etat =
	let Balle ((posx, posy), (dirx, diry)) = balle
	in if collision_rectangle balle raquette_position (raquette_width, raquette_height)
	then
		(* inversion de y *)
		Balle ((posx+.dirx, (float_of_int raquette_height)+.balle_radius), (dirx, -.diry))
	else
		avancer_balle balle g_etat

(* accélère légèrement la balle *)
let accelerer_balle (Balle (pos, (dirx, diry))) =
	Balle (pos, (min (dirx*.1.0005) (dirx+.0.005), min (diry*.1.0005) (diry+.0.005)))


(* Fait avancer la balle, détecte les collisions et supprime les blocs détruits *)
let detecter_collisions () =
	while true do
		let start_time = Unix.gettimeofday () in
			begin
				(* Ce programme ne s'exécute que toutes les 16ms,
				 * soit à une fréquence de 60Hz (fréquence de rafraichissement de l'écran).
				 * Attention: le temps n'est malheureusement pas monotonique ici, on suppose
				 * que ça ne posera pas tro pde problèmes *)
				while Unix.gettimeofday () -. start_time < 1./.60. do
					GreenThreadsState.yield ();
				done;
				let State (LocalState (terrain, balle, raquette), g_etat) = GreenThreadsState.get ()
				in begin
					(* détection et suppression des blocs sur le chemin de la balle *)
					let blocs_collisionants = collision balle terrain
					in let terrain = supprimer_blocs terrain blocs_collisionants
					in let balle = accelerer_balle (detecter_collision_raquette balle raquette g_etat)
					in GreenThreadsState.send (State (LocalState (terrain, balle, raquette), g_etat))
				end;
			end;
	done; GreenThreadsState.exit ()

let detecter_fin_du_jeu () =
	begin
		while true do
			let State (LocalState (_, Balle ((_, y), _), _), _) = GreenThreadsState.get ()
			in if y < 0. then
				(* l'utilisateur a perdu *)
				(* TODO: mieux gérer les cas où la vitesse permet au vecteur direction
				 * d'excéder la taille de la raquette, et donc de déclarer game over sans
				 * raison *)
				GreenThreadsState.stop_scheduler ()
			else
				GreenThreadsState.yield ();
		done;
		(* la fonction 'exit' ne sera jamais exécutée mais est nécessaire pour que la fonction
		 * soit valide du point de vue de son type *)
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
	in let balle = Balle ((float_of_int (size_win_x/2), (float_of_int raquette_height)+.balle_radius), (2.5, 2.5))
	in let raquette = Raquette ((size_win_x-raquette_width)/2, 0)
	in let etat_initial = (State (LocalState (Terrain terrain, balle, raquette), GlobalState (size_win_x, size_win_y)))
	in begin
		GreenThreadsState.scheduler [boucle_evenementielle; detecter_collisions; detecter_fin_du_jeu; dessiner] etat_initial;
		print_endline "Game over !";
	end
