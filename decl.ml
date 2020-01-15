open Scheduler
open Graphics

type integer = | Int of int | Infinity;;
type properties = {color: color; value: int}

type position = int * int
type floating_position = float * float
type direction = float * float
type vitesse_deplacement = float * float


(* La brique a une durée de vie, et elle sera supprimée (à la charge de l'appelant)
* lorsque celle-ci atteindra 'Some 0'.
* Note: sa durée de vie peut aussi être infinie, avec 'None' *)
type brique = { position : position; lifetime : integer ; properties : properties};;



(* une liste (probablement chaînée, suivant le niveau d'optimisation du compilateur),
* bien que non idéale au niveau des performances, a
* l'avantage de permettre des insertions/suppressions en O(1),
* or ces opérations ont lieu fréquemment dans notre jeu *)
type terrain = brique list

type balle = { pos: floating_position; direction : direction};;

type raquette = { position: position; vitesse_deplacement: vitesse_deplacement};;

type local_etat = { terrain : terrain ; balle : balle; raquette: raquette; nb_vies : int };;

type global_etat = { window_size : int * int; score : int};;

type etat = {global_etat: global_etat; local_etat: local_etat;};;


(* Module d'ordonnancement coopératif des différentes tâches *)
module GreenThreadsState = GreenThreads (struct type shift = etat end)

(* fonctions utilitaires *)
let add_tuple (x, y) (x_prime, y_prime) = (x+.x_prime, y+.y_prime)
let sub_tuple (x, y) (x_prime, y_prime) = (x-.x_prime, y-.y_prime)
let tuple_to_float (x, y) = (float_of_int x, float_of_int y)
let tuple_to_int (x, y) = (int_of_float x, int_of_float y)
