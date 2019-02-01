(**************************************************************************)
(*                                                                        *)
(*                  Adrien Blassiau, projet IPF 2018-2019                 *)
(*                                                                        *)
(*      Interfaces d'un foncteur pour la construction d'une enveloppe     *)
(*      convexe en dimension 2.                                           *)
(*                                                                        *)
(**************************************************************************)

module type OrderedTypeConvexHull = sig
  type t

  val compare : t -> t -> int
  val compare_x : t -> t -> int
  val compare_y : t -> t -> int
  val get_angle : t -> t -> float
  val get_tournant : t -> t -> t -> float
  val print : t -> unit
end

module type C = sig
  type point

  val print_pivot : point -> unit
  val compare_graham_angle : point -> point -> point  -> int
  val get_pivot : point list -> point
  val remove_pivot : point list -> point -> point list
  val get_pts_coeff_list : point list -> point list * point
  val sort_pts_list : point list -> point list
  val manage_stack : point My_stack.stack -> point -> point My_stack.stack
  val get_convex_hull : point list -> point list

end

module MakeConvexHull (C : OrderedTypeConvexHull) : (C with type point = C.t)