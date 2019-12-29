open Scheduler

type lifetime = int option
type properties = NilProp

type position = int * int
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

type balle = Balle: position * direction -> balle

type raquette = Raquette: position -> raquette

type win_size = int * int

type local_state = LocalState: terrain * balle * raquette -> local_state
(* TODO: handle resize ? *)
type global_state = GlobalState: win_size -> global_state
type state = State: local_state * global_state -> state


(* Module d'ordonnancement coopératif des différentes tâches *)
module GreenThreadsState = GreenThreads (struct type shift = state end)
