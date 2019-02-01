(**************************************************************************)
(*                                                                        *)
(*                  Adrien Blassiau, projet IPF 2018-2019                 *)
(*                                                                        *)
(*      Interface de fonctions utiles et inclassables dont certaines      *)
(*      que l'on retrouve dans des versions plus récentes d'OCaml         *)
(*      que celle présente sur les machines de l'école                    *)
(*                                                                        *)
(**************************************************************************)

exception EmptyList
exception IsNone

val hd_and_tail : 'a list -> ('a * 'a list)

val is_some : 'a option -> bool

val is_none : 'a option -> bool

val get_some : 'a option -> 'a

val norme_carre : (float * float) -> (float * float) -> float

val norme : (float * float) -> (float * float) -> float

val split : 'a list -> ('a list * 'a option * 'a list)

val shuffle : 'a list -> 'a list