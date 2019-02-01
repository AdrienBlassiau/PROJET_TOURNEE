(**************************************************************************)
(*                                                                        *)
(*                  Adrien Blassiau, projet IPF 2018-2019                 *)
(*                                                                        *)
(*      Interfaces d'un module de villes composés de constructeurs,       *)
(*      getters, setters et d'opérations utiles sur les villes            *)
(*                                                                        *)
(**************************************************************************)

type ville

val construct_ville : string * float * float -> ville
val deconstruct_ville : ville -> string * float * float
val get_name : ville -> string
val get_x : ville -> float
val get_y : ville -> float
val set_name : ville -> string -> ville
val set_x : ville -> float -> ville
val set_y : ville -> float -> ville
val print_ville_info : ville -> unit
val compare_name : ville -> ville -> int
val compare_x : ville -> ville -> int
val compare_y : ville -> ville -> int
val get_angle : ville -> ville -> float
val get_dist : ville -> ville -> float
val get_dist_lazy : ville -> ville -> float
val get_tournant : ville -> ville -> ville -> float

module OrderedTypeVille :
sig
  type t = ville
  val construct : (string * float * float) -> t
  val compare : t -> t -> int
  val compare_x : t -> t -> int
  val compare_y : t -> t -> int
  val get_x : t ->  float
  val get_y : t ->  float
  val get_name : t -> string
  val get_angle : t -> t -> float
  val get_tournant : t -> t -> t -> float
  val print : t -> unit
  val print_name : t -> unit
  val get_infos : t -> string * float * float
  val get_dist : ville -> ville -> float
  val get_dist_lazy : ville -> ville -> float
end

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
