(**************************************************************************)
(*                                                                        *)
(*                  Adrien Blassiau, projet IPF 2018-2019                 *)
(*                                                                        *)
(*      Corps et Interfaces d'un foncteur d'arbre k-d, avec diverses      *)
(*      fonctions permettant notamment d'effectuer une recherche d'un     *)
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

module MakeKDTree (N : OrderedTypeKdTree) : (K with type node = N.t) = struct

  exception Not_found
  exception Impossible

  type node = N.t
  type kd_tree =
    | Leaf
    | Node of kd_tree * bool * node * bool * (node -> node -> int) * bool * bool * kd_tree


  let empty = Leaf

  (* [is_empty l] retourne un booléen qui vaut vrai si l'arbre k-d est vide et
     faux dans le cas échéant.

     @requires  l est un arbre k-d quelconque
     @ensures   le résultat est booléen
  *)
  let is_empty l = l = empty

  (* [creation l status] retourne l'arbre k-d associé à la liste de points passée
     en paramètres. status est un booléen qui indique si les points doivent
     être visités ou non

     @requires  l est une liste de points du plan non vide et status quelconque
     @ensures   l'arbre respecte les propriétés d'un arbre k-d (voir rapport ...)
     @raises    Impossible si l est vide
  *)
  let creation l status =
    let rec creation_interne l axis status =
      let cmp_fun_1, cmp_fun_2 =
        (if axis then N.compare_x,N.compare_y else N.compare_y,N.compare_x)
      in
      let l =  List.sort cmp_fun_1 l in
      match l with
      | a::b::c::d::q ->
        let lg,pivot_opt,ld = Useful.split l in
        let pivot = Useful.get_some pivot_opt in
        let node_g =
          if lg != [] then creation_interne lg (not axis) status else Leaf
        in
        let node_d =
          if ld != [] then creation_interne ld (not axis) status else Leaf
        in
        Node (node_g, status, pivot, axis , cmp_fun_1, status, status, node_d)
      | a::b::c::[] ->
        let node_g =
          Node (Leaf, status, a, not axis ,cmp_fun_2, status, status, Leaf)
        in
        let node_d =
          Node (Leaf, status, c, not axis ,cmp_fun_2, status, status, Leaf)
        in
        Node (node_g, status, b, axis , cmp_fun_1, status, status, node_d)
      | a::b::[] ->
        let node_g = Node (Leaf, status, a, not axis ,cmp_fun_2, status, status, Leaf) in
        let node_d = Leaf in
        Node (node_g, status, b, axis , cmp_fun_1, status, status, node_d)
      | a::[] ->
        let node_g = Leaf in
        let node_d = Leaf in
        Node (node_g, status, a, axis , cmp_fun_1, status, status, node_d)
      | [] -> raise Impossible
    in
    creation_interne l true status

  (* [print_kd_tree kdt] affiche l'arbre k-d de manière très schématique
     sur la sortie standard

     @requires  kdt un arbre k-d quelconque
     @ensures   affiche l'arbre sur la sortie standard
  *)
  let print_kd_tree kdt =
    let rec print_kd_tree_interne kdt prof = match kdt with
      | Leaf -> ()
      | Node (node_g, foll_g, pivot, axis , cmp_fun, tournee, foll_d, node_d) ->
        let blank_prof = String.make (2*prof) '-' in
        print_string blank_prof ;
        N.print_name pivot ;
        print_newline ();
        print_kd_tree_interne node_g (prof+1) ;
        print_kd_tree_interne node_d (prof+1)
    in print_kd_tree_interne kdt 1


  (* [fold_kd_tree f kdt acc] fold sur l'arbre k-d
     @requires  kdt un arbre k-d quelconque, f la fonction a appliquer sur ses
                nœuds et acc l'accumulateur
     @ensures   le résultat est du type de acc
  *)
  let rec fold_kd_tree f kdt acc =
    match kdt with
    | Leaf -> acc
    | Node (node_g,_,pivot, axis,_,_,_,node_d) ->
      fold_kd_tree f node_d (f pivot axis (fold_kd_tree f node_g acc))

  (* [print_kd_tree_plan kdt draw_fun] affiche les plans de l'arbre k-d sur la
     fenêtre graphique
     @requires  kdt un arbre k-d quelconque et draw_fun la fonction d'affichage
     @ensures   affiche l'arbre sur la fenêtre graphique
  *)
  let print_kd_tree_plan kdt draw_fun =
    let plan_t =
      fold_kd_tree (fun pivot axis acc ->
          let _,x,y = N.get_infos pivot in
          let x,y = int_of_float x, int_of_float y in
          (if axis then (x,0,x,1000) else (0,y,1000,y))::acc ) kdt []
    in draw_fun plan_t


  (* [change_status kdt city] change le statut d'un point contenu dans l'arbre.
     Si le statut est vrai, le point sera visité, sinon il ne le sera pas

     @requires  kdt quelconque et city présent dans l'arbre
     @ensures   retourne le même arbre avec un point visitable supplémentaire
     @raises    Not_found si le nœud n'est pas trouvé
  *)
  let rec change_status kdt city = match kdt with
    | Leaf -> raise Not_found
    | Node (node_g, foll_g, pivot, axis, cmp_fun, tournee, foll_d, node_d) ->
      if N.compare pivot city = 0 then
        Node (node_g, foll_g, pivot, axis ,cmp_fun, true, foll_d, node_d)
      else if cmp_fun city pivot > 0 then
        Node (node_g, foll_g, pivot, axis ,cmp_fun, tournee,true, change_status node_d city)
      else
        Node (change_status node_g city, true, pivot, axis ,cmp_fun, tournee, foll_d, node_d)


  (**
     Les 3 fonctions qui suivent sont des fonctions annexes pour la recherche
      du ppv. Elles sont commentées mais ne seront donc pas directement testées
   **)

  (* [test_new_best_complet tournee dist_with_best dist_with_current pivot city]
     teste si le point visité courant peut être le nouveau ppv
     (pour la première partie de l'énoncé)

     @requires  arguments quelconque
     @ensures   retourne un booléen
  *)
  let test_new_best_complet tournee dist_with_best dist_with_current pivot city =
    tournee &&
    ((dist_with_best >= 0. &&
      (dist_with_best >= dist_with_current)) || dist_with_best < 0.) &&
    N.compare pivot city != 0

  (* [test_new_best_non_complet test_fun tournee dist_with_best
      dist_with_current pivot city]
     teste si le point visité courant peut être le nouveau ppv en prenant en
     compte la présence d'un lien possible entre deux points
     (pour la deuxième partie de l'énoncé)

     @requires  arguments quelconques
     @ensures   retourne un booléen
  *)
  let test_new_best_non_complet test_fun tournee dist_with_best
      dist_with_current pivot city =
    let name_city, name_pivot = N.get_name city, N.get_name pivot in
    let first_test =
      test_new_best_complet tournee dist_with_best dist_with_current pivot city
    in
    let second_test = test_fun name_city name_pivot in
    first_test && second_test

  (* [split_hyperplane city pivot axis dist_with_best]
     fonction qui teste si le cercle, dont le centre est le point dont on cherche
     le ppv et le rayon la distance avec le ppv, coupe la droite
     (horizontale ou verticale selon axis) contenant le pivot (plus de détails
     dans le rapport)

     @requires  arguments quelconques
     @ensures   retourne un booléen
  *)
  let split_hyperplane city pivot axis dist_with_best =
    let _,x,y = N.get_infos city in
    let _,pivot_x,pivot_y = N.get_infos pivot in
    let dist_with_current_proj =
      if axis then Useful.norme_carre (pivot_x,y) (x,y)
      else Useful.norme_carre (x,pivot_y) (x,y)
    in dist_with_best >= dist_with_current_proj || dist_with_best < 0.


  (* [find_nearest_neighbour_aux city current_best dist_with_best pivot_rec
      tournee_rec kdt test_new_best_fun]
     fonction de recherche du ppv d'un point passé en argument, à partir d'un
     arbre k-d

     @requires  arguments quelconques sauf dist_with_best négatif
     @ensures   retourne le plus proche voisin sans modifier l'arbre k-d
  *)
  let rec find_nearest_neighbour_aux city current_best dist_with_best pivot_rec
      tournee_rec kdt test_new_best_fun =
    let find_new_best foll pivot tournee node_1 node_2 axis =
      let current_best, dist_with_best =
        if foll
        then find_nearest_neighbour_aux city
            current_best dist_with_best pivot tournee node_1 test_new_best_fun
        else current_best, dist_with_best
      in
      let dist_with_current = N.get_dist_lazy city pivot in
      let current_best, dist_with_best =
        if test_new_best_fun tournee dist_with_best dist_with_current pivot city
        then pivot, dist_with_current
        else current_best, dist_with_best
      in
      let split = split_hyperplane city pivot axis dist_with_best in
      if split
      then find_nearest_neighbour_aux
          city current_best dist_with_best pivot tournee node_2 test_new_best_fun
      else current_best, dist_with_best
    in
    match kdt with
    | Leaf ->
      let dist_with_current = N.get_dist_lazy city pivot_rec in
      if test_new_best_fun tournee_rec dist_with_best dist_with_current pivot_rec city
      then pivot_rec, dist_with_current
      else current_best, dist_with_best
    | Node (node_g, foll_g, pivot, axis,cmp_fun, tournee, foll_d, node_d) ->
      if cmp_fun city pivot >= 0
      then find_new_best foll_d pivot tournee node_d node_g axis
      else find_new_best foll_g pivot tournee node_g node_d axis

  (* [find_nearest_neighbour_non_complet city kdt test_fun]
     fonction de recherche du ppv d'un point passé en argument, à partir d'un
     arbre k-d, pour la partie 2 de l'énoncé

     @requires  arguments quelconques
     @ensures   retourne le plus proche voisin sans modifier l'arbre k-d
  *)
  let find_nearest_neighbour_non_complet city kdt test_fun =
    let test_new_best_fun = test_new_best_non_complet test_fun in
    let current_best, dist_with_best =
      find_nearest_neighbour_aux city city (-1.) city true kdt test_new_best_fun
    in
    current_best, dist_with_best

  (* [find_nearest_neighbour_complet city kdt]
     fonction de recherche du ppv d'un point passé en argument, à partir d'un
     arbre k-d, pour la partie 1 de l'énoncé

     @requires  arguments quelconques
     @ensures   retourne le plus proche voisin sans modifier l'arbre k-d
  *)
  let find_nearest_neighbour_complet city kdt =
    let test_new_best_fun = test_new_best_complet in
    let current_best, dist_with_best =
      find_nearest_neighbour_aux city city (-1.) city true kdt test_new_best_fun
    in
    current_best, dist_with_best
end
