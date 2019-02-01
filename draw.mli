(**************************************************************************)
(*                                                                        *)
(*                  Adrien Blassiau, projet IPF 2018-2019                 *)
(*                                                                        *)
(*      Interfaces d'un foncteur d'affichage graphique de la tournée      *)
(*      utilisant la bibliothèque graphique Graphics d'OCaml              *)
(*                                                                        *)
(**************************************************************************)

module type OrderedTypeSet = sig
  type t
  val compare : t -> t -> int
  val get_infos : t -> (string * float * float)
end

module type P = sig
  type node
  val draw_graph : (int * int) list -> node list -> unit
  val draw_graph_with_modif : (int * int) list -> node list -> node list -> unit
  val draw_graph_with_plan : node list -> (int * int * int * int) list -> unit
end

module MakeDrawer (N : OrderedTypeSet) : (P with type node = N.t)