(**************************************************************************)
(*                                                                        *)
(*              Adrien Blassiau, projet IPF 2018-2019                     *)
(*                                                                        *)
(*      Interfaces des fonctions de tests utilisées dans le projet.       *)
(*      Ce sont pour la grande majorité celles données en TD ...          *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

exception Error of string * exn

val test_with :
  ('b -> 'c -> 'd) -> 'b -> 'c -> float * 'd

val do_test : string * (unit -> 'a) * 'a * (Format.formatter -> 'a -> unit) -> unit

val fprintf_bool : Format.formatter -> bool -> unit

val fprintf_int : Format.formatter -> int -> unit

val fprintf_exn : Format.formatter -> exn -> unit

val fprintf_pts : Format.formatter -> (string * float * float) -> unit

val generate_random_points : int -> (string * float * float) list