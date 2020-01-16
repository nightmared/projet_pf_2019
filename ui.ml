open Graphics
open Plateau
open Decl



let draw_rect (x, y) (w, h) = draw_rect x y w h
let fill_rect (x,y) (w, h) = fill_rect x y w h

(* retourne 'vrai' si le point est dans le rectangle *)
let est_dans_rectangle (posx, posy) (rect_x, rect_y) (rect_width, rect_height) =
	posx >= rect_x && posx <= rect_x + rect_width && posy >= rect_y && posy <= rect_y + rect_height

let collision_rectangle_haut_droite (balle: balle) rect rect_size =
	est_dans_rectangle (tuple_to_int (add_tuple balle.pos (add_tuple (balle.direction) (balle_radius, balle_radius)))) rect rect_size

let collision_rectangle_haut_gauche (balle: balle) rect rect_size =
	est_dans_rectangle (tuple_to_int (add_tuple balle.pos (add_tuple (balle.direction) (-.balle_radius, balle_radius)))) rect rect_size

let collision_rectangle_bas_droite (balle : balle) rect rect_size =
	est_dans_rectangle (tuple_to_int (add_tuple balle.pos (add_tuple (balle.direction) (balle_radius, -.balle_radius)))) rect rect_size

let collision_rectangle_bas_gauche (balle : balle) rect rect_size =
	est_dans_rectangle (tuple_to_int (add_tuple balle.pos (add_tuple (balle.direction) (-.balle_radius, -.balle_radius)))) rect rect_size

(* On vérifie les collisions de la manière suivante (la version précédente faisait
 * du 'lancer de rayon' pour détecter les collisions, mais ce n'est pas très pratique
 * puisque notre balle ne se réduit pas à un seul point *)
let collision_rectangle (balle: balle) rect rect_size =
	(* on vérifie les collisions avec les quatres extrêmes de la 'bounding box' du cercle *)
	(collision_rectangle_bas_gauche balle rect rect_size
	|| collision_rectangle_bas_droite balle rect rect_size
	|| collision_rectangle_haut_gauche balle rect rect_size
	|| collision_rectangle_haut_droite balle rect rect_size)


let collision balle (terrain : terrain) =
	List.filter
		(fun  (brique : brique) -> collision_rectangle balle (brique.position) (brique_width, brique_height))
		terrain

let etat_initial window_size =
	{ 
		etat_local = etat_local_initial (window_size);
		etat_global = { window_size = window_size; score = 0}
	}

let abaisser_duree_de_vie brique =
	match brique.lifetime with
	| Infinity -> Some brique
	| Int old_lifetime ->
			if old_lifetime = 1
			then None
			else Some { brique with lifetime = Int (old_lifetime - 1) }

(* supprime une liste de blocs du terrain *)
let supprimer_blocs (terrain : terrain) (l: brique list) =
		List.fold_left
			(fun acc e -> 
				let e_prime = abaisser_duree_de_vie e
				in List.filter_map
					(fun x -> if x = e then e_prime else Some(x))
					acc
			)
			terrain
			l

let dessiner_terrain (liste_blocs : terrain) =
	List.iter (fun (brique : brique) ->
		set_color brique.properties.color;
		fill_rect brique.position (brique_width,brique_height);
		set_color foreground;
		draw_rect brique.position (brique_width,brique_height);
	) liste_blocs

let dessiner_balle (balle: balle) =
	let (x, y) = tuple_to_int (balle.pos)
	in draw_circle x y (int_of_float balle_radius)

let dessiner_raquette (raquette: raquette) = draw_rect raquette.position (raquette_width, raquette_height)

let avancer_balle (balle: balle) (etat: etat) : balle =
	let (x, y) = balle.pos
	in let (dx, dy) = balle.direction
	in let (win_size_x, win_size_y) = etat.etat_global.window_size
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
	in  { pos = (x, y); direction = (dx, dy)}

(* déplace la raquette si possible *)
let deplacer_raquette ({ position = (x, y); _}) window_size gauche =
	if gauche then
		{
			position = (if x-raquette_offset < 0 then (0, y) else (x-raquette_offset, y));
			vitesse_deplacement = (-.(float_of_int raquette_offset), 0.)
		}
	else
		{
			position = (if x+raquette_offset >= window_size then (window_size-raquette_width, y) else (x+raquette_offset, y));
			vitesse_deplacement = (float_of_int raquette_offset, 0.)
		}

(* modifie l'état en fonction des mouvements de la souris *)
let gerer_entree_souris (etat: etat) x =
	begin
		let raquette = etat.etat_local.raquette
		in let (posx, _) = raquette.position
		in let (win_size_x, _) = etat.etat_global.window_size
		in let new_raquette =
			(if x > posx+raquette_width then
				deplacer_raquette raquette win_size_x false
			else if x < posx then
				deplacer_raquette raquette win_size_x true
				else raquette)
		in if new_raquette <> raquette then
			{ etat with etat_local = { etat.etat_local with raquette = new_raquette} }
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
let faire_evoluer_balle (balle: balle) (raquette: raquette) (etat: etat) =
	let (posx, _) = balle.pos
	in let (dirx, diry) = balle.direction
	in if collision_rectangle balle raquette.position (raquette_width, raquette_height)
	then
		let (raquette_vitesse_x, _) = raquette.vitesse_deplacement
		(* inversion de y et accélération de la vitesse de déplacement horizontale *)
		in {
			pos = (posx+.dirx, (float_of_int raquette_height)+.balle_radius);
			direction = (dirx+.raquette_vitesse_x/.10., -.diry)
		}
	else
		avancer_balle balle etat

(* accélère légèrement la balle *)
let accelerer_balle ({pos = pos; direction = (dirx, diry)}: balle) =
	{
		pos = pos;
		direction = (min (dirx*.1.0005) 4., min (diry*.1.0005) 4.)
	}

let rebond_brique balle (briques: brique list) = 
	match briques with 
	| [] -> balle
	| brique::_ -> 
		let (posx, posy) = balle.pos
		and (dirx, diry) = balle.direction
		and (posbx, posby) = brique.position
		in 
		if (collision_rectangle balle (posbx, posby) (brique_width, brique_height))
		then
			if (posx >= (float_of_int (posbx+brique_width))) && (posy <= (float_of_int (posby+brique_height)) && (posy >= (float_of_int (posby))))
				|| (posx <= (float_of_int (posbx)) && (posy <= (float_of_int (posby+brique_height))) && (posy >= (float_of_int (posby))))
			(* Répartition de l'espace autour de la brique en 8 zones à la manière d'un quadrillage 3x3 (la brique au centre)
			si la balle se trouve dans la zone directement à droite ou à gauche de la brique, alors rebond sur une surface verticale*)
			then 
			 	{ 
					pos = (posx-.dirx, posy+.diry);
				  direction = (-.dirx, diry)
				}
			else (* sinon, rebond sur surface horizontale *)
				{
					pos = (posx+.dirx, posy-.diry) ;
					direction = (dirx, -.diry)
				}
			(* à l'heure actuelle, les zones correspondants aux coins de la brique ne sont pas traitées... *)
		else (* pas de rebond, on re change rien *)
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
				  let local_state = etat.etat_local in
					let balle = local_state.balle 
					and terrain = local_state.terrain
					and raquette = local_state.raquette 
					(* détection et suppression des blocs sur le chemin de la balle *)
					in let blocs_collisionants = collision balle terrain
					in let balle = rebond_brique (accelerer_balle (faire_evoluer_balle balle raquette etat)) blocs_collisionants
					in let terrain = supprimer_blocs terrain blocs_collisionants
					in let score = 
					List.fold_left (+) etat.etat_global.score (List.map (fun (bloc:brique) -> 
						bloc.properties.value
					) blocs_collisionants) 
					 
					in let new_state = 
					{  etat_local =  
							{etat.etat_local with terrain = terrain ; balle = balle};
						etat_global = 
							{etat.etat_global with score = score}
					}

					in GreenThreadsState.send new_state
				end;
			end;
	done; GreenThreadsState.exit ()

let detecter_fin_du_jeu () =
	begin
		while true do
			let etat = GreenThreadsState.get ()
			in let etat_local = etat.etat_local 
			in let (_, y) = etat_local.balle.pos
			in if y < 0. then
				let nb_vies = etat_local.nb_vies
				in (if nb_vies = 1 then
					(* l'utilisateur a perdu *)
					(* TODO: mieux gérer les cas où la vitesse permet au vecteur direction
					 * d'excéder la taille de la raquette, et donc de déclarer game over sans
					 * raison *)
					GreenThreadsState.stop_scheduler ()
				else
					(* réinitialisation du jeu avec mise à jour du nombre de parties restantes *)
					let new_etat = {
						etat with
						etat_local = {
							(etat_local_initial etat.etat_global.window_size)
							with
								terrain = etat_local.terrain;
								nb_vies = nb_vies - 1
						}
					}
					in GreenThreadsState.send new_etat)
			else
				GreenThreadsState.yield ();
		done;
		(* la fonction 'exit' ne sera jamais exécutée mais est nécessaire pour que la fonction
		 * soit valide du point de vue de son type *)
		GreenThreadsState.exit ()
	end

let dessiner_score score (w, h) = 
	moveto (w/50) (h/50);
	set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
	draw_string "Score : ";
	draw_string (string_of_int score)

let dessiner_nb_vies nb_vie (w, h) = 
	moveto (w - 150) (h/50);
	set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
	draw_string "# Vies : ";
	draw_string (string_of_int nb_vie)

(* Dessine les différents objets à l'écran *)
let dessiner () =
	begin
		while true do
			let etat  = (GreenThreadsState.get ()) 
			in let etat_local = etat.etat_local 
			in begin
				clear_graph ();
				dessiner_terrain etat_local.terrain;
				dessiner_balle etat_local.balle;
				dessiner_raquette etat_local.raquette;
				dessiner_score etat.etat_global.score etat.etat_global.window_size;
				dessiner_nb_vies etat.etat_local.nb_vies etat.etat_global.window_size;
				synchronize ();
			end;
			GreenThreadsState.yield ();
		done;
		GreenThreadsState.exit ()
	end

let draw_string_centered text = 
	let w,h = text_size text in
	let x, y = current_point () in 
	moveto (x - w/2) (y- h/2);
	draw_string text

(* 'main' du programme, crée un état initial et lance l'ordonnanceur *)
let run () =
	let win = open_graph " 640x480"
	in let size_win_x = size_x win
	in let size_win_y = size_y win
	in begin
		GreenThreadsState.scheduler [boucle_evenementielle; detecter_collisions; detecter_fin_du_jeu; dessiner] (etat_initial (size_win_x, size_win_y));
		clear_graph();
		moveto (size_win_x / 2) (size_win_y / 2);
		set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
		draw_string_centered "Game over!";
		synchronize ();
	end
