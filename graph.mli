(**************************************************************************)
(*                                                                        *)
(*                  Adrien Blassiau, projet IPF 2018-2019                 *)
(*                                                                        *)
(*      Interfaces d'un foncteur de graphe composés d'opérations          *)
(*      utiles au projet et prenant en paramètre un module possédant      *)
(*      une fonction de comparaison et une fonction d'affichage           *)
(*                                                                        *)
(**************************************************************************)

module type OrderedTypeGraph = sig
  type t
  val compare : t -> t -> int
  val print : t -> unit
end


module type G = sig
  type node

  module NodeMap : Map.S with type key = node
  module NodeSet : Set.S

  type graph

  val empty : graph
  val is_empty : graph -> bool
  val succs : node -> graph -> NodeSet.t

  val exists : graph -> node -> node -> bool

  val construct_graph : (node * node list) list -> graph

  val add_node : node -> graph -> graph
  val remove_node : node -> graph -> graph
  val fold_node : (node -> 'a -> 'a) -> graph -> 'a -> 'a

  val add_edge : node -> node -> graph -> graph
  val remove_edge : node -> node -> graph -> graph
  val fold_edge : (node -> node -> 'a -> 'a) -> graph -> 'a -> 'a

  val print_graph : graph -> unit

  val count_node : graph -> int
  val count_edge : graph -> int

end

module MakeGraph (N : OrderedTypeGraph) : (G with type node = N.t)