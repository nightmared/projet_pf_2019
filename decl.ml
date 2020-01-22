open Scheduler
open Graphics

type integer = | Int of int | Infinity;;
type bonus = OneMoreLife;;
type properties = {color: color; value: int; bonus: bonus option}


type position = int * int
type floating_position = float * float
type direction = float * float
type vitesse_deplacement = float * float


(* La brique a une durée de vie, et elle sera supprimée (à la charge de l'appelant)
* lorsque celle-ci atteindra 'Some 0'.
* Note: sa durée de vie peut aussi être infinie, avec 'None' *)
type brique = { position : floating_position; lifetime : integer ; properties : properties};;



(* une liste (probablement chaînée, suivant le niveau d'optimisation du compilateur),
* bien que non idéale au niveau des performances, a
* l'avantage de permettre des insertions/suppressions en O(1),
* or ces opérations ont lieu fréquemment dans notre jeu *)
type terrain = brique list

type balle = { pos: floating_position; direction : direction};;

type raquette = { position: floating_position; vitesse_deplacement: vitesse_deplacement};;

type etat_local = { terrain : terrain ; balle : balle; raquette: raquette; nb_vies : int };;

type etat_global = { window_size : float * float; score : int};;

type etat = {etat_global: etat_global; etat_local: etat_local;};;

(* Description d'une collision : 
  - point de contact
  - vecteur normale à la surface de contact
*)
type collision = (float*float) * (float*float);;


(* Module d'ordonnancement coopératif des différentes tâches *)
module GreenThreadsState = GreenThreads (struct type shift = etat end)

(* fonctions utilitaires *)
(* produit scalaire entre deux vecteurs *)
let ( |$ ) (x, y) (x', y') = x*.x' +. y *. y';;
(* somme de deux vecteurs *)
let ( +$ ) (x, y) (x', y') = (x+.x', y+.y');;
(* différence de deux vecteurs *)
let ( -$ ) (x, y) (x', y') = (x-.x', y-.y');;
let ( *: ) a (x,y) = (a*.x, a*.y);;
let distance_carre p1 p2 = let x = p2 -$ p1 in (x |$ x);;


let int_of_float2 (x,y) = int_of_float x, int_of_float y;;
let float_of_int2 (x,y) = float_of_int x, float_of_int y;;

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

