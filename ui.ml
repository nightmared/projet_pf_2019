open Graphics

open Decl

let brique_width = 25
let brique_height = 15
let brique_border = 10

let raquette_width = 35
let raquette_height = 15
(* qtté de déplacement de la raquette lors d'une entrée utilisateur *)
let raquette_offset = 10

let balle_radius = 6.


let draw_rect (x, y) (w, h) = draw_rect x y w h

(* retourne 'vrai' si le point est dans le rectangle *)
let est_dans_rectangle (posx, posy) (rect_x, rect_y) (rect_width, rect_height) =
	posx >= rect_x && posx <= rect_x + rect_width && posy >= rect_y && posy <= rect_y + rect_height

let collision_rectangle_haut_droite balle rect rect_size =
	est_dans_rectangle (tuple_to_int (add_tuple (balle_get_pos balle) (add_tuple (balle_get_dir balle) (balle_radius, balle_radius)))) rect rect_size
let collision_rectangle_haut_gauche balle rect rect_size =
	est_dans_rectangle (tuple_to_int (add_tuple (balle_get_pos balle) (add_tuple (balle_get_dir balle) (-.balle_radius, balle_radius)))) rect rect_size
let collision_rectangle_bas_droite balle rect rect_size =
	est_dans_rectangle (tuple_to_int (add_tuple (balle_get_pos balle) (add_tuple (balle_get_dir balle) (balle_radius, -.balle_radius)))) rect rect_size
let collision_rectangle_bas_gauche balle rect rect_size =
	est_dans_rectangle (tuple_to_int (add_tuple (balle_get_pos balle) (add_tuple (balle_get_dir balle) (-.balle_radius, -.balle_radius)))) rect rect_size

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
	List.filter
		(fun brique -> collision_rectangle balle (brique_get_pos brique) (brique_width, brique_height))
		terrain

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

let etat_initial (size_win_x, size_win_y) =
	let terrain = gen_terrain size_win_x size_win_y
	in let balle = Balle ((float_of_int (size_win_x/2), (float_of_int raquette_height)+.balle_radius), (4.5, 4.5))
	in let raquette = Raquette (((size_win_x-raquette_width)/2, 0), (0., 0.))
	in State (LocalState (Terrain terrain, balle, raquette), GlobalState ((size_win_x, size_win_y), 3))


(* supprime une liste de blocs du terrain *)
let supprimer_blocs (Terrain terrain) l =
	Terrain
		(List.fold_left (fun acc e -> List.filter_map (fun x -> if x = e then None else Some(x)) acc) terrain l)


let dessiner_terrain (Terrain liste_blocs) =
	List.iter (fun brique ->
		draw_rect (brique_get_pos brique) (brique_width, brique_height)
	) liste_blocs

let dessiner_balle balle =
	let (x, y) = tuple_to_int (balle_get_pos balle)
	in draw_circle x y (int_of_float balle_radius)

let dessiner_raquette raquette = draw_rect (raquette_get_pos raquette) (raquette_width, raquette_height)

let avancer_balle balle etat =
	let (x, y) = balle_get_pos balle
	in let (dx, dy) = balle_get_dir balle
	in let (win_size_x, win_size_y) = etat_get_win_size etat
	in let (x, dx) =
		(if x+.dx > (float_of_int win_size_x)-.balle_radius then
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
let deplacer_raquette (Raquette ((x, y), _)) win_size gauche =
	if gauche then
		Raquette ((if x-raquette_offset < 0 then (0, y) else (x-raquette_offset, y)), (-.(float_of_int raquette_offset), 0.))
	else
		Raquette ((if x+raquette_offset >= win_size then (win_size-raquette_width, y) else (x+raquette_offset, y)), (float_of_int raquette_offset, 0.))

(* modifie l'état en fonction des mouvements de la souris *)
let gerer_entree_souris etat x =
	begin
		let raquette = etat_get_raquette etat
		in let (posx, _) = raquette_get_pos raquette
		in let (win_size_x, _) = etat_get_win_size etat
		in let new_raquette =
			(if x > posx+raquette_width then
				deplacer_raquette raquette win_size_x false
			else if x < posx then
				deplacer_raquette raquette win_size_x true
				else raquette)
		in if new_raquette <> raquette then
			etat_update_raquette etat new_raquette
		else
			etat
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

(* détecte une collision entre la balle et la raquette, et fait avancer la balle *)
let faire_evoluer_balle balle raquette etat =
	let (posx, _) = balle_get_pos balle
	in let (dirx, diry) = balle_get_dir balle
	in if collision_rectangle balle (raquette_get_pos raquette) (raquette_width, raquette_height)
	then
		let (raquette_vitesse_x, _) = raquette_get_speed raquette
		(* inversion de y et accélération de la vitesse de déplacement horizontale *)
		in Balle ((posx+.dirx, (float_of_int raquette_height)+.balle_radius), (dirx+.raquette_vitesse_x/.10., -.diry))
	else
		avancer_balle balle etat

(* accélère légèrement la balle *)
let accelerer_balle (Balle (pos, (dirx, diry))) =
	Balle (pos, (min (dirx*.1.0005) 5., min (diry*.1.0005) 5.))

let rebond_brique balle briques = 
	if List.length briques > 0
	then
		let brique = List.hd briques
		in let (posx, posy) = balle_get_pos balle
		in let (dirx, diry) = balle_get_dir balle
		in let (posbx, posby) = brique_get_pos brique
		in if (collision_rectangle balle (posbx, posby) (brique_width, brique_height))
		then
			if (posx >= (float_of_int (posbx+brique_width))) && (posy <= (float_of_int (posby+brique_height)) && (posy >= (float_of_int (posby))))
				|| (posx <= (float_of_int (posbx)) && (posy <= (float_of_int (posby+brique_height))) && (posy >= (float_of_int (posby))))
			(* Répartition de l'espace autour de la brique en 8 zones à la manière d'un quadrillage 3x3 (la brique au centre)
			si la balle se trouve dans la zone directement à droite ou à gauche de la brique, alors rebond sur une surface verticale*)
			then 
				Balle ((posx-.dirx, posy+.diry), (-.dirx, diry))
			else (* sinon, rebond sur surface horizontale *)
				Balle ((posx+.dirx, posy-.diry), (dirx, -.diry))
			(* à l'heure actuelle, les zones correspondants aux coins de la brique ne sont pas traitées... *)
		else (* pas de rebond, on re change rien *)
			balle
	else
		balle

(* Fait avancer la balle, détecte les collisions et supprime les blocs détruits *)
let detecter_collisions () =
	while true do
		let start_time = Unix.gettimeofday () in
			begin
				(* Ce programme ne s'exécute que toutes les 16ms,
				 * soit à une fréquence de 60Hz (fréquence de rafraichissement de l'écran).
				 * Attention: le temps n'est malheureusement pas monotonique ici, on suppose
				 * que ça ne posera pas trop de problèmes *)
				while Unix.gettimeofday () -. start_time < 1./.60. do
					GreenThreadsState.yield ();
				done;
				let etat = GreenThreadsState.get ()
				in begin
					let balle = etat_get_balle etat
					in let terrain = etat_get_terrain etat
					(* détection et suppression des blocs sur le chemin de la balle *)
					in let blocs_collisionants = collision balle terrain
					in let balle = rebond_brique (accelerer_balle (faire_evoluer_balle balle (etat_get_raquette etat) etat)) blocs_collisionants
					in let terrain = supprimer_blocs terrain blocs_collisionants
					in GreenThreadsState.send (etat_update_balle (etat_update_terrain etat terrain) balle)
				end;
			end;
	done; GreenThreadsState.exit ()

let detecter_fin_du_jeu () =
	begin
		while true do
			let etat = GreenThreadsState.get ()
			in let (_, y) = balle_get_pos (etat_get_balle (etat))
			in if y < 0. then
				let nb_vies = etat_get_nb_vies etat
				in (if nb_vies = 1 then
					(* l'utilisateur a perdu *)
					(* TODO: mieux gérer les cas où la vitesse permet au vecteur direction
					 * d'excéder la taille de la raquette, et donc de déclarer game over sans
					 * raison *)
					GreenThreadsState.stop_scheduler ()
				else
					(* réinitialisation du jeu avec mise à jour du nombre de parties restantes *)
					let etat = etat_update_nb_vies (etat_initial (etat_get_win_size etat)) (nb_vies-1)
					in GreenThreadsState.send etat)
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
			let LocalState (terrain, balle, raquette) = etat_get_local (GreenThreadsState.get ())
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
	in begin
		GreenThreadsState.scheduler [boucle_evenementielle; detecter_collisions; detecter_fin_du_jeu; dessiner] (etat_initial (size_win_x, size_win_y));
		print_endline "Game over !";
	end
