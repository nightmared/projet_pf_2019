type t_Zero
type t_Succ
type t_Inf

(* La brique a une durée de vie,
et elle sera supprimé lorsque celle-ci atteindra Zero.
Note: sa durée de vie peut aussi être infinie, avec 't_Inf' *)
type ('lifetime, 'properties) brique = 'lifetime * 'properties