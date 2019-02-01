(**************************************************************************)
(*                                                                        *)
(*                  Adrien Blassiau, projet IPF 2018-2019                 *)
(*                                                                        *)
(*      Corps et Interfaces d'un foncteur de graphe composés              *)
(*      d'opérations utiles au projet et prenant en paramètre un          *)
(*      module possédant une fonction de comparaison et une fonction      *)
(*      d'affichage                                                       *)
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

module MakeGraph (N : OrderedTypeGraph) : (G with type node = N.t) = struct

  module NodeMap = Map.Make(N)
  module NodeSet = Set.Make(N)

  type node = N.t
  type graph = NodeSet.t NodeMap.t


  let empty = NodeMap.empty

  (* [is_empty g] retourne un booléen qui vaut vrai si le graphe est vide et
     faux dans le cas échéant.

     @requires  g est un graphe quelconque
     @ensures   le résultat est booléen
  *)
  let is_empty g = NodeMap.is_empty g

  (* [succs k g] retourne le contenu du graphe g sous la clé k.

     @requires g est un graphe quelconque et k quelconque
     @ensures  le résultat est un NodeSet.t
     @raises   Not_Found si la clé fournie k n'est pas dans g
  *)
  let succs k g = NodeMap.find k g

  (* [exists g src dst] retourne un booléen qui vaut vrai si le graphe possède
     un nœud de clé src associé à un NodeSet.t dans lequel on trouve dst
     et faux dans le cas échéant.

     @requires g est un graphe quelconque, src et dst quelconque
     @ensures  le résultat est un booléen
  *)
  let exists g src dst =
    try
      let _ = NodeSet.find dst (NodeMap.find src g) in true
    with Not_found -> false

  (* [construct_graph list_ville_with_road] retourne un graphe dont les nœuds
     sont construits à partir de list_ville_with_road qui est une liste de couples
     de nœuds associés à une liste de nœuds.

     @requires list_ville_with_road quelconque, peut être vide
     @ensures  le résultat est un graphe tel que chaque clé est le nœud du couple
               et le contenu correspondant est la liste de nœuds du couple
               transformée en un set
  *)
  let construct_graph list_ville_with_road =
    List.fold_left (fun acc (city,roads)->
        let new_set = NodeSet.of_list roads in
        NodeMap.add city new_set acc) empty list_ville_with_road

  (* [add_node n g] retourne un graphe g auquel on a rajouté le nœud n.

     @requires g et n quelconques
     @ensures  le résultat est un graphe avec une clé en plus si celle rentrée en
               paramètre n'était pas déjà présente dans le graphe
  *)
  let add_node n g =
    if NodeMap.mem n g then g
    else NodeMap.add n NodeSet.empty g

  (* [remove_node n g] retourne un graphe g auquel on a enlevé le nœud n en tant
     que clé et en tant que contenu.

     @requires g et n quelconques
     @ensures  le résultat est un graphe avec le nœud n en moins en tant que clé
     et que contenu
  *)
  let remove_node n g =
    let new_g = NodeMap.remove n g in
    NodeMap.fold (
      fun m m_succs g_prov ->
        NodeMap.add m (NodeSet.remove n m_succs) g_prov
    ) new_g empty

  (* [add_edge src dst g] retourne un graphe g auquel on a rajouté une arête,
     c'est à dire un lien formalisé par la clé src et par le contenu dst.

     @requires src, dst et g quelconques
     @ensures  le résultat est un graphe avec src en plus en tant que clé si elle
               n'était pas déjà présentes et dst en tant que contenu supplémentaires
               de cette clé si il n'était pas déjà présent.
  *)
  let add_edge src dst g =
    let src_succs =
      try NodeMap.find src g
      with Not_found -> NodeSet.empty in
    let new_succs = NodeSet.add dst src_succs in
    let g2 = NodeMap.add src new_succs g in
    add_node dst g2

  (* [remove_edge src dst g] retourne un graphe g auquel on a enlevé une arête,
     c'est à dire un lien formalisé par la clé src et par le contenu dst.

     @requires src, dst et g quelconques
     @ensures  le résultat est un graphe avec dst en tant que contenu supplémentaires
               de src en moins, si il était présent
  *)
  let remove_edge src dst g =
    try
      let src_succs = (NodeMap.find src g) in
      let new_succs = NodeSet.remove dst src_succs in
      NodeMap.add src new_succs g
    with Not_found -> g

  (* [fold_node f g v0] applique la fonction f sur tout les nœuds de g avec comme
     accumulateur v0.

     @requires f ,g et v0 quelconques
     @ensures  le résultat est construit à partir de tous les nœuds du graphe
               auxquels on a appliqué la fonction f
  *)
  let fold_node f g v0 =
    NodeMap.fold (fun n n_succs acc -> f n acc) g v0

  (* [fold_node f g v0] applique la fonction f sur tout les arêtes de g avec comme
     accumulateur v0.

     @requires f ,g et v0 quelconques
     @ensures  le résultat est construit à partir de tous les arêtes du graphe,
               c'est à dire tout les couples (src,dst), auxquels on a appliqué
               la fonction f
  *)
  let fold_edge f g v0 =
    NodeMap.fold (
      fun src src_succs acc -> NodeSet.fold (fun dst acc_src ->
          f src dst acc_src) src_succs acc
    ) g v0

  (* [count_node g] retourne le nombre de nœuds du graphe, c'est à dire
     le nombre de clés

     @requires g quelconque
     @ensures  le résultat est un entier supérieur à 0
  *)
  let count_node g = fold_node (fun _ acc -> 1 + acc) g 0

  (* [count_edge g] retourne le nombre d’arêtes du graphe, c'est à dire
     le nombre de couples (src,dst)

     @requires g quelconque
     @ensures  le résultat est un entier supérieur à 0
  *)
  let count_edge g = fold_edge (fun _ _ acc -> 1 + acc) g 0

  (* [print_graph g] affiche sur la sortie standard une représentation du
     graphe

     @requires g quelconque
     @ensures  affiche le résultat sur la sortie standard
  *)
  let print_graph g = NodeMap.iter (fun n acc ->
      print_newline ();
      N.print n ;
      NodeSet.fold (fun city _ -> N.print city) acc ()) g ; print_newline ()

end
