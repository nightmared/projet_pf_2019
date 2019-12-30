open Scheduler

type lifetime = int option
type properties = NilProp

type position = int * int
type floating_position = float * float
type direction = float * float


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

type raquette = Raquette: position -> raquette

type win_size = int * int

type local_etat = LocalState: terrain * balle * raquette -> local_etat
(* TODO: handle resize ? *)
type global_etat = GlobalState: win_size -> global_etat
type etat = State: local_etat * global_etat -> etat


(* Module d'ordonnancement coopératif des différentes tâches *)
module GreenThreadsState = GreenThreads (struct type shift = etat end)

(* fonctions utilitaires *)
let add_tuple (x, y) (x_prime, y_prime) = (x+.x_prime, y+.y_prime)
let sub_tuple (x, y) (x_prime, y_prime) = (x-.x_prime, y-.y_prime)
let tuple_to_float (x, y) = (float_of_int x, float_of_int y)
let tuple_to_int (x, y) = (int_of_float x, int_of_float y)
