type t_Zero;;
type t_Succ;;
type t_Inf;;


type position = float * float;;
type vitesse = float;;


(* La brique a une durée de vie,
et elle sera supprimé lorsque celle-ci atteindra 't_Zero'.
Note: sa durée de vie peut aussi être infinie, avec 't_Inf' *)
type ('lifetime, 'properties) brique = position * 'lifetime * 'properties;;


(* Une liste personnalisée pour contenir des objets arbitraires *)
type _ brique_liste =
    | Nil: unit brique_liste
    | Cons: ('e, 'f) brique * 'b brique_liste -> (('e, 'f) brique * 'b) brique_liste

(* une liste (probablement, suivant le niveau d'optimisation du compilateur)
chaînée, bien que non idéale au niveau des performances, a
l'avantage de permettre des insertions/suppressions en O(1),
or ces opérations ont lieu fréquemment dans notre jeu *)
type 'a terrain = 'a brique_liste;;

type balle = position * vitesse;;

Ui.run ();;