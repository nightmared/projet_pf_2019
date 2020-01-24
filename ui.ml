open Graphics
open Drawing
open Plateau
open Decl
open Float

let raquette_fraction = 10.

(* facteurs affectant la sévérité de l'accélération/décélération de la balle à cause des bonus *)
let fact_accel = 1.5
let fact_decel = 0.66

(* facteurs affectant la sévérité de l'agrandissement/rétrécissement de la raquette à cause des bonus *)
let fact_sizeup = 1.5
let fact_sizedown = 0.75

exception Collision_ingerable of cercle * aabb


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
    if (normale |$ normale) = 0.0 then raise (Collision_ingerable (circle,aabb))
    else
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
    let aabb = brique |> brique_to_aabb  and
     cercle = { centre = balle.pos; r = balle_radius} in
     (brique, collision_cercle_aabb cercle aabb)
  ) terrain;;

(* Initialisation de l'état local et de l'état global *)
let etat_initial window_size =
  {
    etat_local = etat_local_initial window_size;
    etat_global = { window_size = window_size; score = 0}
  }

(* Change la couleur de la brique pour refléter sa durée de vie *)
let update_color brique' lifetime =
  { brique' with properties = {brique'.properties with color = (List.nth Plateau.type_briques (lifetime-1)).color}}

(* Retourne une brique dont la durée de vie a été réduit de un *)
let abaisser_duree_de_vie brique =
  match brique.lifetime with
  | Infinity -> brique
  | Int old_lifetime -> update_color { brique with lifetime = Int (old_lifetime - 1) } (old_lifetime);;

(* Est ce que la brique n'a plus de point de vie ? *)
let est_brique_morte brique = brique.lifetime = Int 0;;

(* Abaisse la durée des blocs impliqué dans une collision *)
let abaisser_duree_de_vie_briques (briques_collisions: (brique * collision option) list) =
  List.map(fun (brique, collision) ->
    (match collision with
    | None -> brique
    | Some _ -> abaisser_duree_de_vie brique)
  ) briques_collisions;;

(* Déplace la raquette si possible *)
let deplacer_raquette ({ position = (x, y); vitesse_deplacement = _; width = wid}: raquette) window_width gauche =
  let raquette_offset = raquette_offset /. raquette_fraction in
  if gauche then
    { position = (if x -. raquette_offset < 0. then (0., y) else (x-.raquette_offset, y));
      vitesse_deplacement = (-.raquette_offset, 0.);
      width = wid}
  else
    { position = (if x +. raquette_offset >= window_width then (window_width-.raquette_width, y) else (x+.raquette_offset, y));
      vitesse_deplacement = (raquette_offset, 0.);
      width = wid}

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
  end;;


(* Accélère légèrement la balle *)
let accelerer_balle (balle : balle) =
  let dx, dy = balle.direction in
  {	balle with 
    direction = dx *. (1.+.(1./.ffrequence)),  dy *. (1.+.(1./.ffrequence)) };;

(* Retourne la balle dans son état suivant 
  Autrement dit, fait avancer la balle et gère les rebonds avec les bords de l'écran *)
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

(* Affiche textuellement l'état de la balle *)
let print_balle {pos=(x,y);direction=(dx,dy)} =
  print_string "Point (";print_float x; print_string " , "; print_float y; print_endline ")";
  print_string "Vitesse (";print_float dx;  print_string " , "; print_float dy; print_endline ")";
  print_newline ();;

(* Détecte une collision entre la balle et la raquette, et fait avancer la balle *)
let rebond_raquette(raquette: raquette) (balle: balle)  =
  let cercle = {centre = balle.pos ; r = balle_radius}
  and aabb = {point = raquette.position; width = raquette.width; height = raquette_height} in
  let collision = collision_cercle_aabb cercle aabb in

  match collision with
  | None -> balle
  | Some(pt_contact,normale) ->
    let new_direction = -. 2. *. (balle.direction |$ normale) *: normale +$ balle.direction +$ (1./.1000.)*:raquette.vitesse_deplacement in
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

(* Retourne le score incrémenté en fonction des briques mortes dans la liste de briques passée en paramètre *)
let score briques score_ini =
  let briques_mortes = List.filter est_brique_morte briques in
  let valeurs = List.map (fun brique -> brique.properties.value) briques_mortes in
  List.fold_left (+) score_ini valeurs;;

(* Retourne le nombre de vies du joueurs incrémenter de 1 si une brique contenant un bonus +1 vie est morte dans la liste de briques passée en paramère *)
let nb_vies nb_vies briques  = 
  let a_bonus_one_more_life = fun (b : brique) -> (b.properties.bonus = Some OneMoreLife) in
  let briques_mortes = List.filter (fun brique -> 
    est_brique_morte brique && a_bonus_one_more_life brique
  ) briques in
  nb_vies + List.length briques_mortes;;

(* Retroune la balle après prise en compte des bonus affectant sa vitesse *)
let bonus_vitesse briques balle =
  let bonus_accel = fun (b : brique) -> (b.properties.bonus = Some SpeedUp) in
  let bonus_decel = fun (b : brique) -> (b.properties.bonus = Some SpeedDown) in
  let briques_accel = List.filter (fun brique -> 
    est_brique_morte brique && bonus_accel brique
  ) briques in
  let briques_decel = List.filter (fun brique ->
    est_brique_morte brique && bonus_decel brique
  ) briques in
  let accel = if (List.length briques_accel) = 0 then 1. else fact_accel**(float_of_int (List.length briques_accel)) in
  let decel = if (List.length briques_decel) = 0 then 1. else fact_decel**(float_of_int (List.length briques_decel)) in
  let new_direction = (accel*.decel) *: balle.direction in
  {pos = balle.pos; direction = new_direction}

(* Retourne la raquette après prise en compte des bonus affectant sa taille *)
let bonus_raquette (raquette:raquette) briques =
  let bonus_sizeup = fun (b : brique) -> (b.properties.bonus = Some SizeUp) in
  let bonus_sizedown = fun (b : brique) -> (b.properties.bonus = Some SizeDown) in
  let briques_sizeup = List.filter (fun brique ->
    est_brique_morte brique && bonus_sizeup brique
    ) briques in
  let briques_sizedown = List.filter (fun brique ->
    est_brique_morte brique && bonus_sizedown brique
    ) briques in
  let sizeup = if (List.length briques_sizeup) = 0 then 1. else fact_sizeup**(float_of_int (List.length briques_sizeup)) in
  let sizedown = if (List.length briques_sizedown) = 0 then 1. else fact_sizedown**(float_of_int (List.length briques_sizedown)) in
  let new_width = raquette.width *. sizeup *. sizedown in
  {position = raquette.position; vitesse_deplacement = raquette.vitesse_deplacement; width = new_width}
  


(* Fait avancer la balle, détecte les collisions et supprime les blocs détruits *)
let detecter_collisions () =
  let st =  wait_next_event [ Poll ] in
  let etat = gerer_entree_souris (GreenThreadsState.get()) (float_of_int st.mouse_x) in
  let etat_local = etat.etat_local in
  let balle = etat_local.balle 
  and terrain = etat_local.terrain
  and raquette = etat_local.raquette	in 
  (* fait avancer la balle *)
  let balle' = balle |> avancer_balle etat in 
  (* récupère le statut collision de chaque brique *)
  (try
    let briques_collisions = collisions terrain balle' in
    let collisions = List.filter_map (fun (_, collision) -> collision) briques_collisions in
    (* fait rebondir la balle sur la raquette et les briques *)
    let balle'' = balle'|> rebond_raquette raquette
                        |> rebond_collisions collisions in

    let briques' = abaisser_duree_de_vie_briques briques_collisions in 
    (* calcul du nouveau score *)
    let score' = score briques' etat.etat_global.score in 
    (* ajout d'une vie au joueur si un bonus +1 vie était dans une brique détruite *)
    let nb_vies' = nb_vies etat_local.nb_vies briques' in
    (* accelération/ déceleration de la balle en fonction des bonus *)
    let balle''' = balle'' |> bonus_vitesse briques' in
    (* Suppression des briques dont la durée de vie est nulle *)
    let terrain' = List.filter (fun b -> not (est_brique_morte b)) briques' in
    (* Modification de la taille de la raquette en fonction des bonus *)
    let raquette' = bonus_raquette etat_local.raquette briques' in

    let state' = {  
      etat_local =  {
        terrain = terrain'; 
        balle = balle''';
        raquette = raquette';
        nb_vies = nb_vies'
      };
      etat_global = { 
        etat.etat_global with score = score'
      }
    } in GreenThreadsState.send state'
  with Collision_ingerable (_, _) ->
    (* provoque artificiellement la fin du jeu *)
     let state' = { etat with etat_local = { etat.etat_local with balle = { balle with pos = (-1., -1.) } } }
    in GreenThreadsState.send state')


(* Décrémente le nombre de vies du joueur et met fin à la partie si le joueur n'a plus de vies ou que toutes les briques on été détruites *)
let detecter_fin_du_jeu () =
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
  else if List.fold_right (fun brique bool -> bool && (brique.lifetime <= Int(0) || brique.lifetime = Infinity)) etat_local.terrain true then
    (* Solution temporaire *)
    GreenThreadsState.stop_scheduler ()
    (* TODO : niveau suivant ou Congratulations *)
  else ()

(* Dessine les différents objets à l'écran *)
let dessiner () =
  let etat  = (GreenThreadsState.get ())
  in let etat_local = etat.etat_local
  in begin
    clear_graph ();
    dessiner_terrain etat_local.terrain;
    dessiner_balle etat_local.balle;
    dessiner_raquette etat_local.raquette;
    dessiner_score etat.etat_global.score etat.etat_global.window_size;
    dessiner_nb_vies etat.etat_local.nb_vies etat.etat_global.window_size;
    synchronize ()
  end

(* 'main' du programme, crée un état initial et lance l'ordonnanceur *)
let run () =
  let _ = Random.self_init () in
  let win = open_graph " 640x480" in
  let window_size = float_of_int (size_x win), float_of_int (size_y win) in
  begin
    auto_synchronize false;
    let tasks = GreenThreadsState.gen_scheduler_list [(detecter_collisions, Some ffrequence); (detecter_fin_du_jeu, None); (dessiner, Some 60.)] in
    GreenThreadsState.scheduler tasks (etat_initial window_size);
    clear_graph();
    let w, h = int_of_float2 window_size in 
    moveto (w / 2, h / 2);
    set_font "-*-fixed-medium-r-semicondensed--25-*-*-*-*-*-iso8859-1";
    draw_string_centered "Game over!";
    print_endline "end";
    synchronize ();
  end
