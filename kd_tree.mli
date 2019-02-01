(**************************************************************************)
(*                                                                        *)
(*                  Adrien Blassiau, projet IPF 2018-2019                 *)
(*                                                                        *)
(*      Interfaces d'un foncteur d'arbre k-d, composés de diverses        *)
(*      fonctions permettant notamment  d'effectuer une recherche d'un    *)
(*      plus proche voisin (ppv ici pour abréger) en log(n)               *)
(*              (Plus de détails dans le rapport ...)                     *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

module type OrderedTypeKdTree = sig
  type t

  val compare : t -> t -> int
  val compare_x : t -> t -> int
  val compare_y : t -> t -> int
  val print_name : t -> unit
  val get_name : t -> string
  val get_infos : t -> string * float * float
  val get_dist_lazy : t ->  t -> float
end

module type K = sig
  exception Not_found
  exception Impossible

  type node
  type kd_tree

  val empty : kd_tree

  val is_empty : kd_tree -> bool

  val creation : node list -> bool -> kd_tree

  val print_kd_tree : kd_tree -> unit

  val fold_kd_tree : (node -> bool -> 'a -> 'a) -> kd_tree -> 'a -> 'a

  val print_kd_tree_plan : kd_tree -> ( (int * int * int * int )  list -> unit) -> unit

  val change_status : kd_tree -> node -> kd_tree

  val find_nearest_neighbour_complet : node -> kd_tree -> (node * float)

  val find_nearest_neighbour_non_complet : node -> kd_tree -> (string -> string -> bool) -> (node * float)
end

module MakeKDTree (N : OrderedTypeKdTree) : (K with type node = N.t)