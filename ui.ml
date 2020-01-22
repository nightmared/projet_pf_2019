open Graphics
open Plateau
open Decl
open Float

let raquette_fraction = 10.

let draw_rect (x, y) (w, h) = draw_rect x y w h
let fill_rect (x,y) (w, h) = fill_rect x y w h

(* Un rectangle aligné avec les axes 
   Le rectangle est représenté par son coin bas gauche, 
   et sa longueur et largeur *)
type aabb = {
  point : float*float;
  width : float;
  height: float
};;

(* Un cercle est représenté par les coordonnées de son centre 
  et son rayon *)
type cercle = {
  centre : float*float;
  r : float
};;


(* Closest float to a range [a,b] *)
let closest_float x a b = 
  if x < a then a 
  else if x > b then b 
  else x;;

(* Retourne le point du rectangle AABB le plus proche de (x,y) 
   Si (x,y) est à l'extérieur du rectangle alors le point est sur un côté du rectangle
   Sinon si il est à l'intérieur alors c'est lui même
*)
let projete_point_sur_aabb (x,y) (aabb:aabb) = 
  let aabb_x, aabb_y = aabb.point in
  let closest_x = closest_float x aabb_x (aabb_x +. aabb.width) 
  and closest_y = closest_float y aabb_y (aabb_y +. aabb.height)
  in (closest_x, closest_y);;

(* Y a t-il collision entre un point et un cercle ? *)
let collision_point_cercle point (cercle : cercle) = 
  distance_carre point cercle.centre <= cercle.r*.cercle.r;;

(* Détecte une collision entre un cercle et un AABB
   Retourne une Option indiquant si il y a une collision et si 
   oui le point de collision, et un vecteur normal
*)
let collision_cercle_aabb (circle:cercle) (aabb:aabb) = 
  let closest_point = projete_point_sur_aabb circle.centre aabb in 
  if collision_point_cercle closest_point circle then 
    let normale = circle.centre -$ closest_point in
    Some (closest_point, (1./.sqrt(normale|$normale))*: normale)
  else 
    None;;


(* Retourne les quatres sommets d'un rectangle *)
let sommets_rectangle (rect:aabb) = 
  let { point=(x,y); width=w; height = h } = rect  in
  [(x,y);
   (x +. w,y);
   (x,y +. h);
   (x +. w,y +. h)];;

(* Est ce que r2 collisionne avec r1 *)
let collision_rectangle_rectangle (r1: aabb) (r2: aabb) = 
  let r1_x, r1_y = r1.point and
      r2_x, r2_y = r2.point in
  not((r2_x >= r1_x +. r1.width)     
    || (r2_x +. r2.width <= r1_x) 
    || (r2_y >= r1_y +. r1.height) 
    || (r2_y +. r2.height <= r1_y));;


(* Liste associative indiquant pour chaque brique si il y a collision avec la balle
  Retourne une liste de pairs brique * collision option où
  on associe associe à chaque brique soit
    * None si il n'y a pas collision avec la balle
    * Some collision si il y a une collision avec la balle
*)
let collisions (terrain : terrain)  balle: (brique * collision option) list=
  List.map (fun (brique : brique) -> 
    let aabb = { point= brique.position; width =brique_width; height =brique_height} and
     cercle = { centre = balle.pos; r = balle_radius} in
     (brique, collision_cercle_aabb cercle aabb)
  ) terrain;;

let etat_initial window_size =
  { 
    etat_local = etat_local_initial window_size;
    etat_global = { window_size = window_size; score = 0}
  }

(* Retourne une brique dont la durée de vie a été réduit de un *)
let abaisser_duree_de_vie brique =
  match brique.lifetime with
  | Infinity -> brique
  | Int old_lifetime ->  { brique with lifetime = Int (old_lifetime - 1) };;

(* Est ce que la brique n'a plus de point de vie ? *)
let est_brique_morte brique = brique.lifetime = Int 0;;

(* Abaisse la durée des blocs impliqué dans une collision *)
let abaisser_duree_de_vie_briques (briques_collisions: (brique * collision option) list) =
  List.map(fun (brique, collision) -> 
    (match collision with 
    | None -> brique
    | Some _ -> abaisser_duree_de_vie brique)
  ) briques_collisions;;

let dessiner_terrain (liste_blocs : terrain) =
  List.iter (fun (brique : brique) ->
    set_color brique.properties.color;
    fill_rect (int_of_float2 brique.position) (int_of_float2 (brique_width,brique_height));
    set_color foreground;
    draw_rect (int_of_float2 brique.position) (int_of_float2 (brique_width,brique_height));
  ) liste_blocs;;

let dessiner_balle (balle: balle) =
  let (x, y) = tuple_to_int (balle.pos)
  in draw_circle x y (int_of_float balle_radius);;

let dessiner_raquette (raquette: raquette) = 
  draw_rect (int_of_float2 raquette.position) (int_of_float2 (raquette_width, raquette_height));;

(* Déplace la raquette si possible *)
let deplacer_raquette ({ position = (x, y); _}: raquette) window_width gauche =
  let raquette_offset = raquette_offset /. raquette_fraction in 
  if gauche then
    { position = (if x -. raquette_offset < 0. then (0., y) else (x-.raquette_offset, y));
      vitesse_deplacement = (-.raquette_offset, 0.) }
  else
    { position = (if x +. raquette_offset >= window_width then (window_width-.raquette_width, y) else (x+.raquette_offset, y));
      vitesse_deplacement = (raquette_offset, 0.) }

(* Modifie l'état en fonction des mouvements de la souris *)
let gerer_entree_souris (etat: etat) x =
  begin
    let raquette = etat.etat_local.raquette
    in let (posx, _) = raquette.position
    in let (win_size_x, _) = etat.etat_global.window_size
    in let new_raquette = (
      if x > posx+.raquette_width/.2. then
        deplacer_raquette raquette win_size_x false
      else if x < posx +.raquette_width/.2.  then
        deplacer_raquette raquette win_size_x true
      else raquette )
    in if new_raquette <> raquette then
      { etat with etat_local = { etat.etat_local with raquette = new_raquette} }
    else
      etat
  end

(* Boucle évènementielle pour dessiner la raquette *)
let rec boucle_evenementielle () =
  let st = wait_next_event [ Poll ]
  in let etat = GreenThreadsState.get ()
  in let new_etat = gerer_entree_souris etat (float_of_int st.mouse_x)
  in begin
    GreenThreadsState.send new_etat;
    GreenThreadsState.continue boucle_evenementielle
  end

(* Accélère légèrement la balle *)
let accelerer_balle (balle : balle) =
  let dx, dy = balle.direction in
  {	balle with 
    direction = dx *. (1.+.(0.01/.ffrequence)),  dy *. (1.+.(0.0001/.ffrequence)) };;


let avancer_balle (etat: etat) (balle: balle) : balle =
  let (x, y) = balle.pos in 
  let (dx, dy) = balle.direction in
  let (win_size_x, win_size_y) = etat.etat_global.window_size in 
  let (x, dx) =
    (if x+.dx > win_size_x-.balle_radius then
      (win_size_x-.balle_radius, -.dx)
    else if (int_of_float (x+.dx)) < 0 then
      (balle_radius, -.dx)
    else
      (x+.dx, dx)) in 
  let (y, dy) = (if y+.dy > win_size_y -.balle_radius then
    ( win_size_y -. balle_radius, -.dy)
  else
    (y+.dy, dy))
  in  { pos = (x, y); direction = (dx, dy)};;

let print_balle {pos=(x,y);direction=(dx,dy)} = 
  print_string "Point (";print_float x; print_string " , "; print_float y; print_endline ")";
  print_string "Vitesse (";print_float dx;  print_string " , "; print_float dy; print_endline ")";
  print_newline ();;

(* Détecte une collision entre la balle et la raquette, et fait avancer la balle *)
let rebond_raquette(raquette: raquette) (balle: balle)  =	
  let cercle = {centre = balle.pos ; r = balle_radius} 
  and aabb = {point =raquette.position; width = raquette_width; height = raquette_height} in 
  let collision = collision_cercle_aabb cercle aabb in 

  match collision with 
  | None -> balle
  | Some(pt_contact,normale) -> 
    let new_direction = -. 2. *. (balle.direction |$ normale) *: normale +$ balle.direction +$ (1./.20.)*:raquette.vitesse_deplacement in
    let dx, dy = new_direction in
    if is_nan dx || is_nan dy then raise Division_by_zero;
    let balle' = {pos = pt_contact+$ balle_radius*:normale; direction=new_direction} in 
    balle';;

(* Détecte une collision entre la balle et les collisions données et renvoie la
   position de la balle après rebond *)
let rebond_collisions (collisions: collision list) (balle: balle) : balle = 
  match collisions with 
  | [] -> balle
  | (pt_contact, normale)::_ -> 
    let new_direction = -. 2. *. (balle.direction |$ normale) *: normale +$ balle.direction in
    { pos = pt_contact +$ balle_radius*:normale; direction = new_direction};;

let score briques score_ini = 
  let briques_mortes = List.filter est_brique_morte briques in 
  let valeurs = List.map (fun brique -> brique.properties.value) briques_mortes in 
  List.fold_left (+) score_ini valeurs;;

(* Fait avancer la balle, détecte les collisions et supprime les blocs détruits *)
let detecter_collisions () =
  while true do
    let start_time = Unix.gettimeofday () in
      begin
        (* Ce programme ne s'exécute que toutes les 16ms,
         * soit à une fréquence de 60Hz (fréquence de rafraichissement de l'écran).
         * Attention: le temps n'est malheureusement pas monotonique ici, on suppose
         * que ça ne posera pas trop de problèmes *)
        while Unix.gettimeofday () -. start_time < 1./. (float_of_int frequence)  do
          GreenThreadsState.yield ();
        done;
        let etat = GreenThreadsState.get ()
        in begin
          let local_state = etat.etat_local in
          let balle = local_state.balle 
          and terrain = local_state.terrain
          and raquette = local_state.raquette	in 
          (* fait avancer la balle *)
          let balle' = balle |> avancer_balle etat in 
          (* récupère le statut collision de chaque brique *)
          let briques_collisions = collisions terrain balle' in
          let collisions = List.filter_map (fun (_, collision) -> collision) briques_collisions in
          (* fait rebondir la balle sur la raquette et les briques *)
          let balle'' = balle'|> rebond_raquette raquette
                              |> rebond_collisions collisions in

          let briques' = abaisser_duree_de_vie_briques briques_collisions in 
          (* calcul du nouveau score *)
          let score' = score briques' etat.etat_global.score in 
          (* Suppression des briques dont la durée de vie est nulle *)
          let terrain' = List.filter (fun b -> not (est_brique_morte b)) briques' in
           
          let state' = {  
            etat_local =  {
              etat.etat_local with terrain = terrain' ; balle = balle''
            };
            etat_global = { 
              etat.etat_global with score = score'
            }
          } in 
          GreenThreadsState.send state'
        end;
      end;
  done; 
  GreenThreadsState.exit ();;


let rec detecter_fin_du_jeu () =
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
      begin
        GreenThreadsState.stop_scheduler ();
        (* la fonction 'exit' ne sera jamais exécutée mais est nécessaire pour que la fonction
         * soit valide du point de vue de son type *)
        GreenThreadsState.exit ()
      end
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
      in begin
        GreenThreadsState.send new_etat;
        GreenThreadsState.continue detecter_fin_du_jeu
      end)
  else
    GreenThreadsState.continue detecter_fin_du_jeu

let dessiner_score score (w, h) = 
  moveto ((int_of_float w)/50) ((int_of_float h)/50);
  set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
  draw_string "Score : ";
  draw_string (string_of_int score)

let dessiner_nb_vies nb_vie (w, h) = 
  moveto ((int_of_float w) - 150) ((int_of_float h)/50);
  set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
  draw_string "# Vies : ";
  draw_string (string_of_int nb_vie)

(* Dessine les différents objets à l'écran *)
let rec dessiner () =
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
    GreenThreadsState.continue dessiner;
  end

let draw_string_centered text = 
  let w,h = text_size text in
  let x, y = current_point () in 
  moveto (x - w/2) (y- h/2);
  draw_string text

(* 'main' du programme, crée un état initial et lance l'ordonnanceur *)
let run () =
  let win = open_graph " 640x480" in
  let window_size = float_of_int (size_x win), float_of_int (size_y win) in
  begin
    auto_synchronize false;
    GreenThreadsState.scheduler [boucle_evenementielle; detecter_collisions; detecter_fin_du_jeu; dessiner] (etat_initial window_size);
    clear_graph();
    let w, h = int_of_float2 window_size in 
    moveto (w / 2) (h / 2);
    set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
    draw_string_centered "Game over!";
    synchronize ();
  end
