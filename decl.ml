type lifetime = int option
type properties = NilProp

type position = float * float;;
type vitesse = float;;


(* La brique a une durée de vie, et elle sera supprimée (à la charge de l'appelant)
lorsque celle-ci atteindra 'Some 0'.
Note: sa durée de vie peut aussi être infinie, avec 'None' *)
type brique = Brique: position * lifetime * properties -> brique;;


(* une liste (probablement chaînée, suivant le niveau d'optimisation du compilateur),
bien que non idéale au niveau des performances, a
l'avantage de permettre des insertions/suppressions en O(1),
or ces opérations ont lieu fréquemment dans notre jeu *)
type terrain = brique list;;

type balle = position * vitesse;;