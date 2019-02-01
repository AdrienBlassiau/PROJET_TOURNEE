(**************************************************************************)
(*                                                                        *)
(*                  Adrien Blassiau, projet IPF 2018-2019                 *)
(*                                                                        *)
(*      Corps d'un module de villes composés de constructeurs, getters,   *)
(*      setters et d'opérations utiles sur les villes                     *)
(*                                                                        *)
(**************************************************************************)

open Useful

type ville = string * float * float

(* [construct_ville (name,x,y)] construction d'une ville

   @requires  name,x,y quelconques
   @ensures   le résultat est une ville
*)
let construct_ville (name,x,y) = (name,x,y)

(* [deconstruct_ville (name, x, y)] déconstruit d'une ville

   @requires  une ville quelconque
   @ensures   le résultat est un triplet
*)
let deconstruct_ville (name,x, y) = (name,x,y)

(* [get_name v] renvoie le nom d'une ville

   @requires  v quelconque
   @ensures   le résultat est le nom de la ville
*)
let get_name (name,_, _)  = name

(* [get_x v] renvoie la coordonnée x d'une ville

   @requires  v quelconque
   @ensures   le résultat est la coordonnée x de la ville
*)
let get_x (_,x,_) = x

(* [get_y v] renvoie la coordonnée y d'une ville

   @requires  v quelconque
   @ensures   le résultat est la coordonnée y de la ville
*)
let get_y (_,_,y) = y


(* [set_name v new_name] renvoie la ville v avec le nom new_name

   @requires  v et new_name quelconques
   @ensures   le résultat est la ville avec son nom changé
*)
let set_name (name,x,y) new_name = (new_name,x,y)

(* [set_x v new_x] renvoie la ville v avec la coordonnée new_x

   @requires  v et new_x quelconques
   @ensures   le résultat est la ville avec sa coordonnée x changée
*)
let set_x (name,x,y) new_x = (name,new_x,y)

(* [set_y v new_y] renvoie la ville v avec la coordonnée new_y

   @requires  v et new_y quelconques
   @ensures   le résultat est la ville avec sa coordonnée y changée
*)
let set_y (name,x,y) new_y = (name,x,new_y)

(* [print_name v] affiche le nom de la ville v sur la sortie standard

   @requires  v quelconque
   @ensures   le résultat est affiché sur la sortie standard
*)
let print_name (name,_,_) = print_string name

(* [print_ville_info v] affiche les informations de la ville sur la sortie
   standard

   @requires  v quelconque
   @ensures   le résultat est affiché sur la sortie standard
*)
let print_ville_info (name,x,y) =
    print_newline ();
    print_string (name^" ("^(string_of_float x)^"/"^(string_of_float y)^") ")

(* [compare_x v1 v2] compare la coordonnée x de deux villes

   @requires  v1 et v2 quelconques
   @ensures   le résultat est un entier
*)
let compare_x (_,x1,_) (_,x2,_) = compare x1 x2

(* [compare_y v1 v2] compare la coordonnée y de deux villes

   @requires  v1 et v2 quelconques
   @ensures   le résultat est un entier
*)
let compare_y (_,_,y1) (_,_,y2) = compare y1 y2

(* [compare_name v1 v2] compare le nom de deux villes

   @requires  v1 et v2 quelconques
   @ensures   le résultat est un entier
*)
let compare_name (n1,_,_) (n2,_,_) = String.compare n1 n2

(* [get_angle v1 v2] donne l'angle entre deux vecteurs

   @requires  v1 et v2 quelconques
   @ensures   le résultat est un flottant
*)
let get_angle (_,x1,y1) (_,x2,y2) = atan2 (y2 -. y1) (x2 -. x1)

(* [get_dist v1 v2] donne la distance entre deux villes

   @requires  v1 et v2 quelconques
   @ensures   le résultat est un flottant
*)
let get_dist (_,x1,y1) (_,x2,y2) = Useful.norme (x1,y1) (x2,y2)

(* [get_dist_lazy v1 v2] donne la distance entre deux villes passée au carré
   (utiliser pour la comparaison de distance pour éviter de faire l'opération
   racine carrée supplémentaire inutilement ...)

   @requires  v1 et v2 quelconques
   @ensures   le résultat est un flottant
*)
let get_dist_lazy (_,x1,y1) (_,x2,y2) = Useful.norme_carre (x1,y1) (x2,y2)

(* [get_tournant v1 v2 v3] indique si l'on effectue un tournant à gauche ou
   à droite (utilisé dans la partie sur l'enveloppe convexe)

   @requires  v1, v2 et v3 quelconques
   @ensures   le résultat est un flottant
*)
let get_tournant (_,x1,y1) (_,x2,y2) (_,x3,y3) =
  (x2 -. x1)*.(y3 -. y1) -. (y2 -. y1)*.(x3 -. x1)


module type OrderedTypeVille = sig
  type t = ville
  val construct : (string * float * float) -> t
  val compare : t -> t -> int
  val compare_x : t -> t -> int
  val compare_y : t -> t -> int
  val get_x : t -> float
  val get_y : t -> float
  val get_name : t -> string
  val get_angle : t -> t -> float
  val get_tournant : t -> t -> t -> float
  val print : t -> unit
  val print_name : t -> unit
  val get_infos : t -> string * float * float
  val get_dist : ville -> ville -> float
  val get_dist_lazy : ville -> ville -> float
end

module OrderedTypeVille = struct
  type t = ville
  let construct = construct_ville
  let compare = compare_name
  let compare_x = compare_x
  let compare_y = compare_y
  let get_x = get_x
  let get_y = get_y
  let get_name = get_name
  let get_angle = get_angle
  let get_tournant = get_tournant
  let print = print_ville_info
  let print_name = print_name
  let get_infos = deconstruct_ville
  let get_dist = get_dist
  let get_dist_lazy = get_dist_lazy
end