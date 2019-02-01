(**************************************************************************)
(*                                                                        *)
(*                  Adrien Blassiau, projet IPF 2018-2019                 *)
(*                                                                        *)
(*      Corps et Interfaces d'un foncteur pour la construction d'une      *)
(*      enveloppe convexe en dimension 2 prenant en paramètre un module   *)
(*      qui implémente divers fonctions utiles à la recherche de          *)
(*      l'enveloppe                                                       *)
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

module MakeConvexHull (N : OrderedTypeConvexHull) : (C with type point = N.t) = struct

  open Useful

  type point = N.t

  (* [print_pivot p] fonction d'affichage du pivot
     (utilisée pour le débogage ...)

     @requires  p quelconque
     @ensures   le résultat est affiché sur la sortie standard
  *)
  let print_pivot p =
    print_endline "Le pivot est :";
    print_endline "**************";
    N.print p ;
    print_endline "**************"

  (* [compare_graham_angle v1 v2 vp] fonction de comparaison de deux de vecteurs
     en fonctions de l'angle qu'ils font avec l’horizontale

     @requires  v1, v2 et vp quelconques
     @ensures   le résultat est un booléen
  *)
  let compare_graham_angle v1 v2 vp =
    let a1 = N.get_angle vp v1 in
    let a2 = N.get_angle vp v2 in
    if a1 < a2 then (-1)
    else if a1 > a2 then 1
    else N.compare_x v1 v2

  (* [get_pivot pts_list] fonction de recherche du pivot, c'est à dire du point
     "le plus bas", parmi une liste de points

     @requires  pts_list non vide
     @ensures   le résultat est un point de la liste
  *)
  let get_pivot pts_list =
    List.fold_left (fun acc elt ->
        if N.compare_y acc elt > 0 then elt
        else if N.compare_y acc elt = 0 then
          if (N.compare_x acc elt > 0) then elt else acc
        else acc )
      (List.hd pts_list) pts_list

  (* [remove_pivot pts_list pivot] fonction de retrait du pivot de la liste de
     points

     @requires  pts_list quelconque
     @ensures   le résultat est la liste d'entrée avec un élément en moins,
                le pivot passé en paramètre d'entrée
  *)
  let rec remove_pivot pts_list pivot = match pts_list with
    | [] -> []
    | t::q ->
      if N.compare pivot t = 0
      then remove_pivot q pivot
      else t::remove_pivot q pivot

  (* [get_pts_coeff_list pts_list] fonction qui renvoie la liste des points sans
     le pivot et le pivot

     @requires  pts_list non vide
     @ensures   le résultat est la liste d'entrée avec un élément en moins
                et le pivot
  *)
  let get_pts_coeff_list pts_list =
    let pivot = get_pivot pts_list in
    let new_pts_list = remove_pivot pts_list pivot in
    new_pts_list, pivot

  (* [sort_pts_list pts_list] fonction qui trie la liste de points selon l'angle
     que chaque vecteur formé par un point de la liste et le pivot
     fait avec l'horizontale

     @requires  pts_list non vide
     @ensures   le résultat est la liste d'entrée triée, avec le pivot
                en premier élément
  *)
  let sort_pts_list pts_list =
    let new_list, pivot = get_pts_coeff_list pts_list in
    pivot::(List.fast_sort (fun v1 v2->
        compare_graham_angle v1 v2 pivot) new_list)

  (* [manage_stack stack current_ele] fonction qui s'occupe de rechercher les points
     appartenant à l'enveloppe convexe

     @requires  stack et current_ele quelconque
     @ensures   le résultat est la pile avec des points encore susceptibles
                d'appartenir à l'enveloppe convexe
  *)
  let rec manage_stack stack current_ele =
    if My_stack.get_size stack < 2 then stack else
      let elem1, elem2 = My_stack.get_first_and_second stack in
      let tournant = N.get_tournant current_ele elem2 elem1 in
      let bool_tournant = tournant <= 0. in
      if bool_tournant then
        let new_stack = snd(My_stack.pop stack) in
        manage_stack new_stack current_ele
      else stack

  (* [get_convex_hull pts_list] fonction qui s'occupe de rechercher l'enveloppe
     convexe d'un ensemble de points

     @requires  pts_list quelconque
     @ensures   le résultat est une liste contenant les points de l'enveloppe
                convexe dont l'ordre de parcours est le sens des aiguilles d'une
                montre et dont le dernier élément est le pivot
  *)
  let get_convex_hull pts_list = match pts_list with
    | [] -> []
    | t::[] -> pts_list
    | _ ->
    let sort_list = sort_pts_list pts_list in
    let stack = My_stack.new_stack in
    let elem1, sort_list = hd_and_tail sort_list in
    let elem2, sort_list = hd_and_tail sort_list in
    let stack = My_stack.push elem2 (My_stack.push elem1 stack) in
    let stack = List.fold_left (fun acc elt ->
        let ville = elt in
        let stack = manage_stack acc ville in
        My_stack.push ville stack) stack sort_list in
    My_stack.fold (fun elt acc -> elt::acc) stack []
end
