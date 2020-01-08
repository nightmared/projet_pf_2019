open Scheduler

type lifetime = int option
type properties = NilProp

type position = int * int
type floating_position = float * float
type direction = float * float
type vitesse_deplacement = float * float


(* La brique a une durée de vie, et elle sera supprimée (à la charge de l'appelant)
* lorsque celle-ci atteindra 'Some 0'.
* Note: sa durée de vie peut aussi être infinie, avec 'None' *)
type brique = Brique: position * lifetime * properties -> brique


(* une liste (probablement chaînée, suivant le niveau d'optimisation du compilateur),
* bien que non idéale au niveau des performances, a
* l'avantage de permettre des insertions/suppressions en O(1),
* or ces opérations ont lieu fréquemment dans notre jeu *)
type terrain = Terrain: brique list -> terrain

type balle = Balle: floating_position * direction -> balle

type raquette = Raquette: position * vitesse_deplacement -> raquette

type win_size = int * int

type nb_vies = int

type local_etat = LocalState: terrain * balle * raquette -> local_etat
(* TODO: handle resize ? *)
type global_etat = GlobalState: win_size * nb_vies -> global_etat
type etat = State: local_etat * global_etat -> etat


let raquette_get_pos (Raquette (pos, _)) = pos
let raquette_get_speed (Raquette (_, speed)) = speed
let balle_get_pos (Balle (pos, _)) = pos
let balle_get_dir (Balle (_, dir)) = dir
let brique_get_pos (Brique (pos, _, _)) = pos
let etat_get_local (State (local, _)) = local
let etat_get_global (State (_, global)) = global
let etat_get_win_size etat = let GlobalState (win_size, _) = etat_get_global etat in win_size
let etat_get_nb_vies etat = let GlobalState (_, nb_vies) = etat_get_global etat in nb_vies
let etat_get_terrain etat = let LocalState (terrain, _, _) = etat_get_local etat in terrain
let etat_get_balle etat = let LocalState (_, balle, _) = etat_get_local etat in balle
let etat_get_raquette etat = let LocalState (_, _, raquette) = etat_get_local etat in raquette
let etat_update_terrain (State ((LocalState (_, balle, raquette)), g_etat)) terrain =
	State (LocalState (terrain, balle, raquette), g_etat)
let etat_update_balle (State ((LocalState (terrain, _, raquette)), g_etat)) balle =
	State (LocalState (terrain, balle, raquette), g_etat)
let etat_update_raquette (State ((LocalState (terrain, balle, _)), g_etat)) raquette =
	State (LocalState (terrain, balle, raquette), g_etat)
let etat_update_nb_vies (State (local, GlobalState (win_size, _))) nb_vies =
	(State (local, GlobalState (win_size, nb_vies)))

(* Module d'ordonnancement coopératif des différentes tâches *)
module GreenThreadsState = GreenThreads (struct type shift = etat end)

(* fonctions utilitaires *)
let add_tuple (x, y) (x_prime, y_prime) = (x+.x_prime, y+.y_prime)
let sub_tuple (x, y) (x_prime, y_prime) = (x-.x_prime, y-.y_prime)
let tuple_to_float (x, y) = (float_of_int x, float_of_int y)
let tuple_to_int (x, y) = (int_of_float x, int_of_float y)
