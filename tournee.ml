(**************************************************************************)
(*                                                                        *)
(*                  Adrien Blassiau, projet IPF 2018-2019                 *)
(*                                                                        *)
(*      Corps et Interfaces d'un foncteur de tournée comportant           *)
(*      un ensemble de fonctions permettant de réaliser les différentes   *)
(*      étapes données dans l'énoncé                                      *)
(*                                                                        *)
(**************************************************************************)

module type T = sig

  module CityGraph : Graph.G
  module Drawer : Draw.P
  module ConvexHull : Convex_hull.C

  type ville
  type tournee
  type city_set
  type kd_tree

  type first_step
  type second_step
  type third_step


  val get_convex_hull : ville list -> ville list

  val print_tournee : ville list -> ville list -> unit

  val print_simple_road : ville list -> unit

  val get_string_and_float_road : ville list -> float * string list

  val get_run : unit -> bool

  val get_input_prameters : unit -> (bool * bool)

  val get_parameters : verbose:bool -> path_from_root_param:string -> unit -> (first_step * second_step * third_step)

  val get_cities : ?complet:bool -> verbose:bool -> path_from_root_city:string -> unit -> (ville list * (string * string list) list)

  val set_kd_tree : ville list -> verbose:bool -> graphic:bool -> kd_tree

  val add_cities_to_tournee : (string * string list) list -> tournee option


  val process_step_1 :
    ?tournee:tournee ->
    ?complet:bool ->
    first_step:first_step ->
    verbose:bool ->
    graphic:bool ->
    ville list ->
    kd_tree ->
    (ville list * city_set * kd_tree * bool)

  val process_step_2 :
    ?tournee:tournee ->
    ?complet:bool ->
    second_step:second_step ->
    verbose:bool ->
    graphic:bool ->
    city_set ->
    ville list  ->
    ville list  ->
    kd_tree ->
    ville list

  val process_step_3 :
    ?tournee:tournee ->
    ?complet:bool ->
    third_step:third_step ->
    verbose:bool ->
    graphic:bool ->
    ville list  ->
    ville list  ->
    bool ->
    ville list

end

module MakeTournee (N : Ville.OrderedTypeVille) : (T with type ville = N.t) = struct

  open Printf
  open Graph
  open Read
  open Graphics
  open Draw
  open Convex_hull
  open City_set
  open Kd_tree

  exception Found of N.t
  exception Wrong_parameters of string

  module String = struct
    type t = string
    let compare = compare
    let print s = print_string (s^" ")
  end

  module CityGraph = MakeGraph(String)
  module Drawer = MakeDrawer(N)
  module ConvexHull = MakeConvexHull(N)
  module CitySet = MakeCitySet(N)
  module KDTree = MakeKDTree(N)
  module Reader = MakeReader(N)

  type ville = N.t
  type tournee = CityGraph.graph
  type city_set = CitySet.city_set
  type kd_tree = KDTree.kd_tree

  type first_step = ONE | HULL
  type second_step = RANDOM | NEAREST | FARTHEST
  type third_step = REPOSITIONNEMENT | INVERSION


  (* [get_convex_hull city_list] récupère l'enveloppe convexe d'un ensemble
     de villes (testée dans le module dédié)

     @requires  city_list quelconque
     @ensures   le résultat est une liste
  *)
  let get_convex_hull city_list =
    ConvexHull.get_convex_hull city_list


  (* [get_a_city city_list] récupère une ville dans la liste de villes et
     la renvoie dans une liste, la liste des routes. Les villes restantes sont
     placées dans un Set

     @requires  city_list quelconque
     @ensures   le résultat est deux listes de villes
  *)
  let get_a_city city_list = match city_list with
    | [] -> [], (CitySet.make_city_set [])
    | _ ->
      let (city, city_list) = Useful.hd_and_tail city_list in
      let road_list = [city] in
      road_list, (CitySet.make_city_set city_list)

  (* [get_city_set city_list road_list] récupère dans un Set les villes à placer
     dans la tournée en retirant celles déjà présentes dans la liste de route

     @requires  city_list et road_list quelconques
     @ensures   le résultat est un Set de villes
  *)
  let get_city_set city_list road_list =
    let city_set_too_big = CitySet.make_city_set city_list in
    CitySet.remove_cities road_list city_set_too_big

  (* [make_loop_road road_list] renvoie la route courante en plaçant la première
     ville à la fin de manière à faire une boucle

     @requires  road_list est non vide
     @ensures   le résultat est une liste augmentée de 1 élément
  *)
  let make_loop_road road_list = road_list@[List.hd road_list]

  (* [get_distance road_list] renvoie la distance de la tournée courante

     @requires  road_list quelconque
     @ensures   le résultat est un flottant positif
  *)
  let rec get_distance road_list = match road_list with
    | [] -> 0.
    | _::[] -> 0.
    | a::b::q -> (N.get_dist a b) +. get_distance (b::q)

  (* [get_full_distance road_list] renvoie la distance totale de la tournée courante

     @requires  road_list non vide
     @ensures   le résultat est un flottant positif
  *)
  let get_full_distance road_list =
    let loop_road = make_loop_road road_list in
    get_distance loop_road

  (* [print_road] simple appel de la fonction d'affichage du module de graphe *)
  let print_road = CityGraph.print_graph

  (* [get_road_list road_list] renvoie la liste de routes sous forme d'une liste
     de doublets.

     @requires  road_list quelconque
     @ensures   le résultat est une liste de doublets
  *)
  let get_road_list road_list =
    List.fold_left (fun acc_list etape ->
        let _,x,y = N.get_infos etape in
        (int_of_float x,int_of_float y)::acc_list) [] road_list

  (* [print_tournee_with_modif road_list city_list changed_nodes]
     affiche la tournée graphiquement en mettant en avant les points qui on
     subit des modifications (points de changed_nodes)

     @requires  road_list, city_list et changed_nodes quelconques
     @ensures   le résultat est affiché sur une fenêtre graphique
  *)
  let print_tournee_with_modif road_list city_list changed_nodes =
    let road_list = get_road_list road_list in
    Drawer.draw_graph_with_modif road_list city_list changed_nodes

  (* [print_tournee road_list city_list] affiche la tournée graphiquement

     @requires  road_list et city_list quelconques
     @ensures   le résultat est affiché sur une fenêtre graphique
  *)
  let print_tournee road_list city_list =
    let road_list = get_road_list road_list in
    Drawer.draw_graph road_list city_list

  (* [print_road road_list] affiche la tournée sur la sortie standard

     @requires  road_list quelconque
     @ensures   le résultat est affiché sur la sortie standard
  *)
  let print_road road_list =
    List.iter ( fun city ->
        let name,_,_ = N.get_infos city in
        print_string (name^" ")) road_list ; print_newline () ; print_newline ()

  (* [print_simple_road road_list] affiche la tournée sur la sortie standard
     selon le formatage de l'énoncé

     @requires  road_list quelconque
     @ensures   le résultat est affiché sur la sortie standard
  *)
  let print_simple_road road_list =
    let loop_road = make_loop_road road_list in
    let distance = get_distance loop_road in
    printf "%f : " distance ; print_road loop_road

  (* [get_int_and_float_road road_list] retourne une route en faisant astraction
     de types utilisés (utilisé dans le module de test)

     @requires  road_list quelconque
     @ensures   le résultat est une couple distance * liste
  *)
  let get_string_and_float_road road_list =
    let loop_road = make_loop_road road_list in
    let loop_road_parse = List.map (fun city -> N.get_name city) loop_road in
    let distance = get_distance loop_road in
    (distance,loop_road_parse)

  (* [choose_random_city_complet city_list] fonction qui choisit de la
     première ville du Set de ville

     @requires  city_list quelconque
     @ensures   le résultat est la première ville de city_list mélangée
  *)
  let choose_random_city_complet city_list =
    let city_list = CitySet.city_set_to_list city_list in
    let city_list_shuffled = Useful.shuffle city_list in
    try
      let random_city = List.hd city_list_shuffled
      in Some random_city
    with
    | Failure _ -> None

  (* [choose_random_city_non_complet city_set kd_tree exist_fun]
     choix de la première ville du Set de ville qui peut être insérée dans la
     tournée.

     @requires  city_set, kd_tree et exist_fun quelconques
     @ensures   le résultat est la première ville de city_list mélangée qui
                peut être insérée dans la tournée
  *)
  let choose_random_city_non_complet city_set kd_tree exist_fun =
    let city_list = CitySet.city_set_to_list city_set in
    let city_list_shuffled = Useful.shuffle city_list in
    try
      List.fold_left(fun acc city ->
          let _, dist_with_best =
            KDTree.find_nearest_neighbour_non_complet city kd_tree exist_fun in
          if dist_with_best < 0. then None
          else raise (Found city) ) None city_list_shuffled
    with Found city -> Some city

  (* [add_road_list_on_kd_tree road_list kd_tree]
     retourne un arbre kd dans lequel se trouve les villes de la tournée, toutes
     marquées comme étant inaccessibles, sauf celles se trouvant dans road_list.
     C'est un prétraitement qui offre ensuite un gain de temps considérable lors
     de l'étape de la recherche du plus proche voisin.

     @requires  road_list et kd_tree tels que kd_tree contient les villes de
                road_list
     @ensures   le résultat est un arbre kd avec le même nombre d'éléments qu'au
                départ
  *)
  let add_road_list_on_kd_tree road_list kd_tree =
    List.fold_left (fun acc ville ->
        KDTree.change_status acc ville) kd_tree road_list

  (* [add_road_list_on_kd_tree road_list kd_tree] fonction qui retourne le même
     kd tree qu'en entrée mais avec la ville city dont l’accessibilité est true.
     Appelée après chaque ajout d'une ville dans la tournée.

     @requires  city et kd_tree tels que kd_tree contient city
     @ensures   le résultat est un arbre kd avec le même nombre d'éléments qu'au
                départ
     @raises    Not_found si le ville n'est pas dans la tournée
  *)
  let add_city_on_kd_tree city kd_tree = KDTree.change_status kd_tree city

  (* [find_best_city_in_tournee_non_complet exist_fun city kd_tree]
     fonction qui retourne le plus proche voisin de la ville city en vérifiant
     qu'il lui est bien lié.

     @requires  city et kd_tree quelconques. exist_fun donne l'existence (ou non)
                d'un lien dans la tournée entre deux villes
     @ensures   le résultat est le plus proche voisin de city dans la tournée
  *)
  let find_best_city_in_tournee_non_complet exist_fun kd_tree city =
    KDTree.find_nearest_neighbour_non_complet city kd_tree exist_fun

  (* [find_best_city_in_tournee_complet city kd_tree]
     fonction qui retourne le plus proche voisin de la ville city

     @requires  city et kd_tree quelconques
     @ensures   le résultat est le plus proche voisin de city dans la tournée
  *)
  let find_best_city_in_tournee_complet kd_tree city =
    KDTree.find_nearest_neighbour_complet city kd_tree

  (* [find_best_city_aux city_set road_list comp find_best_city_fun]
     fonction qui réalise la recherche de la ville
     (non présente dans la tournée donc dans city_set) dont la distance minimale
     aux villes de la tournée partielle (donc dans road_list) est minimale ou
     maximale, en fonction du signe stocké dans comp. find_best_city_fun est une
     fonction de recherche du plus proche voisin.

     @requires  city_set et road_list doivent avoir une intersection vide,
                comp doit être > ou < et find_best_city_fun doit être une des
                deux fonctions définies plus haut
     @ensures   le résultat est respecte la consigne de l'énoncé
  *)
  let find_best_city_aux city_set road_list comp find_best_city_fun =
    let best_city_opt, _ =
      CitySet.fold (fun city_not_in_tournee (best_city,best_dist) ->
          let _,new_dist = find_best_city_fun city_not_in_tournee in
          if (best_dist < 0. || comp new_dist best_dist) && (new_dist >= 0.)
          then Some city_not_in_tournee, new_dist
          else best_city,best_dist
        ) city_set (None, (-1.))
    in
    best_city_opt

  (* [find_best_city_non_complet city_set road_list kd_tree exist_fun comp]
     fonction qui appelle la fonction de de recherche de la ville incorporer
     dans le cas d'un graphe non complet

     @requires  city_set et road_list doivent avoir une intersection vide,
                comp doit être > ou < et kd_tree contient toutes les
                villes.
     @ensures   le résultat est respecte la consigne
  *)
  let find_best_city_non_complet city_set road_list kd_tree exist_fun comp =
    let find_best_city_fun =
      find_best_city_in_tournee_non_complet exist_fun kd_tree
    in
    find_best_city_aux city_set road_list comp find_best_city_fun

  (* [find_best_city_complet city_set road_list kd_tree comp ]
     fonction qui appelle la fonction de de recherche de la ville incorporer
     dans le cas d'un graphe complet

     @requires  city_set et road_list doivent avoir une intersection vide,
                comp doit être > ou < et kd_tree contient toutes les
                villes.
     @ensures   le résultat est respecte la consigne
  *)
  let find_best_city_complet city_set road_list kd_tree comp =
    let find_best_city_fun = find_best_city_in_tournee_complet kd_tree in
    find_best_city_aux city_set road_list comp find_best_city_fun

  (* [insert_best_city_in_road best_city road_list a c]
     fonction qui insert une ville (best_city) dans la tournée (road_list) entre
     a et c

     @requires  best_city, road_list, a et c quelconques
     @ensures   le résultat est road_list augmentée de 1 élément si a et b sont
                dans la liste, et augmentée de 0 élément sinon
  *)
  let rec insert_best_city_in_road best_city road_list a c =
    match road_list with
    | [] -> []
    | test_a::[] -> test_a::best_city::[]
    | test_a::test_c::q ->
      if N.compare test_a a = 0 && N.compare test_c c = 0
      then test_a::best_city::test_c::q
      else test_a::(insert_best_city_in_road best_city (test_c::q) a c)

  (* [insert_best_city_in_road_with_ar best_city road_list a]
     fonction qui insert une ville (best_city) dans la tournée (road_list) entre
     a et a.

     @requires  best_city, road_list, a  quelconques
     @ensures   le résultat est road_list augmentée de 2 éléments si a est dans
                la liste, et augmentée de 0 élément sinon
  *)
  let rec insert_best_city_in_road_with_ar best_city road_list a =
    match road_list with
    | [] -> []
    | test_a::q ->
      if N.compare test_a a = 0
      then test_a::best_city::test_a::q
      else test_a::(insert_best_city_in_road_with_ar best_city q a)

  (* [get_road_diff_simple a b c]
     fonction qui indique l'augmentation de la taille de la tournée après
     insertion de b entre a et c

     @requires  c quelconque et a et b se suivant dans la tournée
     @ensures   le résultat est positif
  *)
  let get_road_diff_simple a b c =
    let ab = N.get_dist a b in
    let bc = N.get_dist b c in
    let ac = N.get_dist a c in
    ab +. bc -. ac

  (* [get_road_diff_a_r a b]
     fonction qui indique l'augmentation de la taille de la tournée après
     insertion de b entre a et a

     @requires  a et b quelconques
     @ensures   le résultat est positif
  *)
  let get_road_diff_a_r a b =
    let ab = N.get_dist a b in
    2.*.ab

  (* [get_road_diff_repo a b c x y]
     fonction qui indique la différence de taille de la tournée avant et après
     une opération de repositionnement. S'il le résultat est négatif, on diminue
     la taille de la tournée, le repositionnement est bénéfique. Sinon il ne
     l'est pas

     @requires  a et b se suivant dans la tournée, de même pour x et y, c quelconque
     @ensures   le résultat est un entier.
  *)
  let get_road_diff_repo a b c x y =
    let ac = N.get_dist a c in
    let xb = N.get_dist x b in
    let by = N.get_dist b y in
    let xy = N.get_dist x y in
    let ab = N.get_dist a b in
    let bc = N.get_dist b c in
    ac +. xb +. by -. xy -. ab -. bc

  (* [get_road_diff_inv a b c d]
     fonction qui indique la différence de taille de la tournée avant et après
     une opération d'inversion. S'il le résultat est négatif, on diminue
     la taille de la tournée, l'inversion est bénéfique. Sinon elle ne
     l'est pas

     @requires  a et b se suivent dans la tournée, de même pour c et d
     @ensures   le résultat est un entier.
  *)
  let get_road_diff_inv a b c d =
    let ac = N.get_dist a c in
    let bd = N.get_dist b d in
    let ab = N.get_dist a b in
    let cd = N.get_dist c d in
    ac +. bd -. ab -. cd

  (* [find_best_road_simple_vs_ar best_city a c exist_fun]
     fonction qui recherche les villes a et c la ou l'insertion d'une ville best_city
     et le plus bénéfique pour une tournée. On vérifie que l'opération est possible
     en s’assurant qu'une route existe entre les villes concernées avec la fonction
     exist_fun. Cette fonction est utilisé pour les graphes non complet

     @requires  a et c se suivent dans la tournée, best_city quelconque et exist_fun
                indique si un lien existe entre deux villes
     @ensures   le résultat est un quintuplet :
                (insertion possible ?, départ, arrivé,
                distance rajoutée à la tournée, boucle ?)
  *)
  let find_best_road_simple_vs_ar best_city a c exist_fun =
    let name_a, name_c, name_city =
      N.get_name a, N.get_name c, N.get_name best_city
    in
    let exist_a = exist_fun name_city name_a in
    let exist_c = exist_fun name_city name_c in
    if not exist_a then
      false, best_city, best_city, -1., false
    else if not exist_c then
      let new_road_diff_ar = (get_road_diff_a_r a best_city) in
      true, a, a, new_road_diff_ar, true
    else
      let new_road_diff_simple = get_road_diff_simple a best_city c in
      let new_road_diff_ar = get_road_diff_a_r a best_city in
      if new_road_diff_simple < new_road_diff_ar then
        true, a, c, new_road_diff_simple, false
      else
        true, a, a, new_road_diff_ar, true

  (* [find_best_road_aux_non_complet best_city road_diff a c
      road_list a_r exist_fun]
     fonction qui recherche les villes a et c la ou l'insertion d'une ville best_city
     et le plus bénéfique pour une tournée road_list. Elle appelle la fonction du
     dessus. Utilisée dans le cas d'un graphe non complet.

     @requires  a et c se suivent dans la tournée, best_city quelconque et exist_fun
                indique si un lien existe entre deux villes. a_r quelconque, road_list
                ne contient pas best_city. road_diff quelconque
     @ensures   le résultat est un triplet avec a_r à vrai si a = c
  *)
  let rec find_best_road_aux_non_complet best_city road_diff a c
      road_list a_r exist_fun =
    match road_list with
    | [] | _::[] -> a,c,a_r
    | new_a::new_c::q ->
      let exist, src, dst, new_road_diff, new_a_r =
        find_best_road_simple_vs_ar best_city new_a new_c exist_fun
      in
      if (road_diff < 0. || new_road_diff < road_diff) && exist
      then find_best_road_aux_non_complet
          best_city new_road_diff src dst (new_c::q) new_a_r exist_fun
      else find_best_road_aux_non_complet
          best_city road_diff a c (new_c::q) a_r exist_fun

  (* [find_best_road_non_complet best_city road_list exist_fun]
     fonction qui effectue la recherche de la meilleure ville en appelant une
     fonction présentée pus haut. Utilisée dans le cas d'un graphe non complet.

     @requires  best_city et road_list disjoints, exist_fun donne l'existence
                d'une route entre deux villes
     @ensures   le résultat est l'endroit où l'insertion est le plus bénéfique
  *)
  let find_best_road_non_complet best_city road_list exist_fun =
    let loop_road = make_loop_road road_list in
    find_best_road_aux_non_complet
      best_city (-1.) best_city best_city loop_road false exist_fun

  (* [find_best_road_without_loop_non_complet best_city road_list exist_fun]
     fonction qui effectue la recherche de l'endroit où placer la meilleur ville
     sans placer la dernière ville de la tournée à la fin. Utilisée pour le
     repositionnement sur un graphe non complet.

     @requires  best_city et road_list disjoints, exist_fun donne l'existence
                d'une route entre deux villes
     @ensures   le résultat est l'endroit où l'insertion est le plus bénéfique
  *)
  let find_best_road_without_loop_non_complet best_city road_list exist_fun =
    find_best_road_aux_non_complet
      best_city (-1.) best_city best_city road_list false exist_fun

  (* [find_and_insert_best_road_non_complet best_city road_list exist_fun]
     fonction qui effectue la recherche de la meilleure ville en l'insérant
     au meilleur endroit, dans le cas d'un graphe non complet

     @requires  best_city et road_list disjoints, exist_fun donne l'existence
                d'une route entre deux villes
     @ensures   road_list voit sa taille augmentée de 0 (pas d'insertion),
                1 (insertion avec boucle) ou 2 (insertion standard)
  *)
  let find_and_insert_best_road_non_complet best_city road_list exist_fun =
    let a, c, a_r = find_best_road_non_complet best_city road_list exist_fun in
    if a_r
    then insert_best_city_in_road_with_ar best_city road_list a
    else insert_best_city_in_road best_city road_list a c

  (* [find_best_road_aux_complet best_city road_diff a c road_list]
     fonction qui recherche les villes a et c la ou l'insertion d'une ville best_city
     et la plus bénéfique pour une tournée road_list. Elle appelle la fonction du
     dessus. Utilisée dans le cas d'un graphe complet.

     @requires  a et c se suivent dans la tournée, best_city quelconque. road_list
                ne contient pas best_city. road_diff quelconque
     @ensures   le résultat sont les deux villes où l'insertion est la meilleure
  *)
  let rec find_best_road_aux_complet best_city road_diff a c road_list =
    match road_list with
    | [] | _::[] -> a,c
    | new_a::new_c::q ->
      let new_road_diff = get_road_diff_simple new_a best_city new_c in
      if road_diff < 0. || new_road_diff < road_diff then
        find_best_road_aux_complet best_city new_road_diff new_a new_c (new_c::q)
      else
        find_best_road_aux_complet best_city road_diff a c (new_c::q)

  (* [find_best_road_complet best_city road_list]
     fonction qui effectue la recherche de la meilleure ville en appelant une
     fonction présentée plus haut. Utilisée dans le cas d'un graphe complet.

     @requires  best_city et road_list disjoints
     @ensures   le résultat est l'endroit où l'insertion est le plus bénéfique
  *)
  let find_best_road_complet best_city road_list =
    let loop_road = make_loop_road road_list in
    find_best_road_aux_complet best_city (-1.) best_city best_city loop_road

  (* [find_best_road_without_loop_complet best_city road_list]
     fonction qui effectue la recherche de l'endroit où placer la meilleur ville
     sans placer la dernière ville de la tournée à la fin. Utilisée pour le
     repositionnement sur un graphe complet.

     @requires  best_city et road_list disjoints
     @ensures   le résultat est l'endroit où l'insertion est le plus bénéfique
  *)
  let find_best_road_without_loop_complet best_city road_list =
    find_best_road_aux_complet best_city (-1.) best_city best_city road_list

  (* [find_and_insert_best_road_complet best_city road_list]
     fonction qui effectue la recherche de la meilleure ville en l'insérant
     au meilleur endroit, dans le cas d'un graphe complet

     @requires  best_city et road_list disjoints
     @ensures   road_list voit sa taille augmentée de 0 (pas d'insertion)
                ou 2 (insertion standard)
  *)
  let find_and_insert_best_road_complet best_city road_list =
    let a, c = find_best_road_complet best_city road_list in
    insert_best_city_in_road best_city road_list a c


  (* [push_best_city exist_fun second_step complet city_set road_list kd_tree
      ~verbose ~graphic]
     fonction qui effectue la recherche de la meilleure ville, son insertion et
     ma mise à jour de l'arbre kd. exist_fun est la fonction qui teste l’existence
     d'un lien entre deux villes, second_step contient le type d'insertion voulu,
     complet indique si le graphe est complet, city_set contient les villes non
     encore placée dans la tournée, road_list les villes de la tournée, kd_tree
     permet de trouver le plus proche voisin plus facilement, ~verbose permet
     d'afficher ou non des infos et graphic permet d'afficher graphiquement la
     tournée

     @requires  best_city et road_list disjoints, kd_tree contient toutes les villes
                de la tournée et autres paramètres quelconques
     @ensures   renvoie une fonction
  *)
  let push_best_city exist_fun second_step complet city_set road_list kd_tree
      ~verbose ~graphic =
    let best_city = Useful.get_some @@
      match second_step with
      | RANDOM ->
        if complet
        then choose_random_city_complet city_set
        else choose_random_city_non_complet city_set kd_tree exist_fun
      | NEAREST ->
        if complet
        then find_best_city_complet city_set road_list kd_tree (<)
        else find_best_city_non_complet city_set road_list kd_tree exist_fun (<)
      | FARTHEST ->
        if complet
        then find_best_city_complet city_set road_list kd_tree (>)
        else find_best_city_non_complet city_set road_list kd_tree exist_fun (>)
    in
    (if verbose then printf "Ville choisie : %s\n" (N.get_name best_city) else ())
  ; let new_kd_tree = add_city_on_kd_tree best_city kd_tree in
    let new_city_set = CitySet.remove_city best_city city_set in
    let new_road_list =
      if complet
      then find_and_insert_best_road_complet best_city road_list
      else find_and_insert_best_road_non_complet best_city road_list exist_fun
    in new_city_set, new_road_list, new_kd_tree

  (* [insert_all_cities city_set road_list cities_list kd_tree tournee complet
      second_step ~verbose ~graphic]
     fonction qui effectue l'insertion de toutes les villes dans la tournée

     @requires  best_city et road_list disjoints, kd_tree contient toutes les
                villes de la tournée et autres paramètres quelconques
     @ensures   road_list contient au moins une fois toutes les villes de la
                tournée
  *)
  let insert_all_cities city_set road_list cities_list kd_tree tournee complet
      second_step ~verbose ~graphic =
    let exist_fun = CityGraph.exists tournee in
    let push_best_city_fun = push_best_city exist_fun second_step complet
        ~verbose ~graphic
    in
    let rec insert_all_cities_aux city_set road_list kd_tree =
      try
        let new_city_set, new_road_list, new_kd_tree =
          push_best_city_fun city_set road_list kd_tree
        in
        insert_all_cities_aux new_city_set new_road_list new_kd_tree
      with
      | Useful.IsNone
      | Not_found -> (if graphic then print_tournee road_list cities_list else ())
                   ; (if verbose
                      then
                        let _ = print_simple_road road_list in
                        printf "Distance totale : %f\n" (get_full_distance road_list)
                      else ())
                   ; road_list
    in
    insert_all_cities_aux city_set road_list kd_tree

  (* [print_avant_apres_repo a b c x y road_diff txt]
     fonction d'affichage de villes participants à un repositionnement

     @requires  a, b, c, x, y, road_diff et txt quelconques
     @ensures   le résultat est affiché sur la sortie standard
  *)
  let print_avant_apres_repo a b c x y road_diff txt=
    print_endline "Avant :";
    N.print_name b ; print_string " -> " ; N.print_name a ; print_string " " ;
    N.print_name c ;
    print_newline () ;
    print_endline "Après :";
    N.print_name b ; print_string " -> " ; N.print_name x ; print_string " " ;
    N.print_name y ;
    print_newline () ;
    print_float road_diff ; print_string (" : "^txt) ; print_newline () ;
    print_newline ()

  (* [print_avant_apres_inv a b c d road_diff txt]
     fonction d'affichage de villes participants à une inversion

     @requires  a, b, c, d, road_diff et txt quelconques
     @ensures   le résultat est affiché sur la sortie standard
  *)
  let print_avant_apres_inv a b c d road_diff txt=
    print_endline "Avant :";
    N.print_name a ; print_string " -> " ; N.print_name b ; print_string " " ;
    N.print_name c; print_string " -> "; N.print_name d; print_newline () ;
    print_endline "Après :";
    N.print_name a ; print_string " -> " ; N.print_name c ; print_string " " ;
    N.print_name b; print_string " -> "; N.print_name d; print_newline () ;
    print_float road_diff ; print_string (" : "^txt) ; print_newline () ;
    print_newline ()

  (* [repositionnement_aux_complet road_list cities_list test count
      changed_nodes ~verbose ~graphic]
     fonction de repositionnement des villes de la tournée. S'arrête lorsqu'il
     n'y a plus de repositionnement possible. Fonction utilisée pour un graphe
     complet.

     @requires  road_list contient au moins une fois toutes les villes de la
                tournée, cities_list contient une fois toute les villes de la
                tournée, autres paramètres quelconques
     @ensures   la nouvelle route est contenue dans road_list, elle est de distance
                inférieure à celle de road_list en entrée de fonction
  *)
  let rec repositionnement_aux_complet road_list cities_list test count
      changed_nodes ~verbose ~graphic =
    let repositionnement_aux_interne_complet road_list test count changed_nodes =
      match road_list with
      | a::b::c::q ->
        let road_list_for_testing = c::q@[a] in
        let x,y = find_best_road_without_loop_complet b road_list_for_testing in
        let road_diff = get_road_diff_repo a b c x y in
        if road_diff < -0.001 then
          let test = true in
          let changed_nodes = b::changed_nodes in
          let new_road = insert_best_city_in_road b road_list_for_testing x y in
          (if verbose then
             let _ = print_avant_apres_repo a b c x y road_diff "OUI" in
             print_road new_road
           else ())
        ; (if graphic then
             let _ =
               print_tournee_with_modif road_list cities_list changed_nodes
             in
             Unix.sleep 1)
        ; repositionnement_aux_complet new_road cities_list test (count-1)
            changed_nodes ~verbose ~graphic
        else
          repositionnement_aux_complet (b::c::q@[a]) cities_list test (count-1)
            changed_nodes ~verbose ~graphic
      | _ -> road_list
    in
    if count <= 0 then
      if not test then road_list
      else let count = List.length road_list in
        repositionnement_aux_interne_complet road_list false count changed_nodes
    else repositionnement_aux_interne_complet road_list test count changed_nodes

  (* [repositionnement_aux_non_complet road_list cities_list test count
      changed_nodes exist_fun ~verbose ~graphic]
     fonction de repositionnement des villes de la tournée. S'arrête lorsqu'il
     n'y a plus de repositionnement possible. Fonction utilisée pour un graphe
     non complet.

     @requires  road_list contient au moins une fois toutes les villes de la
                tournée, cities_list contient une fois toute les villes de la
                tournée, autres paramètres quelconques
     @ensures   la nouvelle route est contenue dans road_list, elle est de distance
                inférieure à celle de road_list en entrée de fonction
  *)
  let rec repositionnement_aux_non_complet road_list cities_list test count
      changed_nodes exist_fun ~verbose ~graphic =
    let repositionnement_aux_interne_non_complet road_list test count
        changed_nodes =
      match road_list with
      | a::b::c::q ->
        let name_a, name_b, name_c = N.get_name a, N.get_name b, N.get_name c in
        let remove_a_r = N.compare a c = 0 in
        if exist_fun name_a name_c || remove_a_r then
          let road_list_for_testing = c::q@[a] in
          let x,y,a_r =
            find_best_road_without_loop_non_complet b road_list_for_testing exist_fun
          in
          let road_diff = get_road_diff_repo a b c x y in
          if road_diff < -0.001 && N.compare b x != 0 && N.compare b y != 0 then
            let test = true in
            let changed_nodes = b::changed_nodes in
            let new_road =
              if a_r
              then insert_best_city_in_road_with_ar b road_list_for_testing x
              else insert_best_city_in_road b road_list_for_testing x y
            in
            let new_road = if remove_a_r then List.tl new_road else new_road in
            (if verbose then
               let _ = print_avant_apres_repo a b c x y road_diff "OUI" in
               print_road new_road
             else ())
          ; (if graphic then
               let _ = print_tournee_with_modif road_list cities_list changed_nodes in
               Unix.sleep 1)
          ; repositionnement_aux_non_complet new_road cities_list test
              (count-1) changed_nodes exist_fun ~verbose ~graphic
          else
            repositionnement_aux_non_complet (b::c::q@[a]) cities_list test
              (count-1) changed_nodes exist_fun ~verbose ~graphic
        else
          repositionnement_aux_non_complet (b::c::q@[a]) cities_list test
            (count-1) changed_nodes exist_fun ~verbose ~graphic
      | _ -> road_list
    in
    if count <= 0 then
      if not test then road_list
      else
        let count = List.length road_list in
        repositionnement_aux_interne_non_complet road_list false count changed_nodes
    else repositionnement_aux_interne_non_complet road_list test count changed_nodes

  (* [find_inversion_complet ~verbose l a b acc]
     fonction qui recherche une inversion qui diminue la taille de la tournée pour
     un couple a b donné. Utilisé pour un graphe complet

     @requires  ~verbose, l, a, b et acc quelconques
     @ensures   on renvoie l'inversion la plus bénéfique trouvée pour un couple
                donné
  *)
  let rec find_inversion_complet ~verbose l a b acc = match l with
    | [] -> b, acc
    | fin::[] -> b, acc@[fin]
    | c::d::q ->
      let road_diff = get_road_diff_inv a b c d in
      if road_diff < -0.001 then
        let _ =
          (if verbose then
             print_avant_apres_inv a b c d road_diff "OUI"
           else ()) in
        c, (List.rev acc)@(b::d::q)
      else
        find_inversion_complet ~verbose (d::q) a b (acc@[c])

  (* [find_inversion_non_complet exist_fun ~verbose l a b acc]
     fonction qui recherche une inversion qui diminue la taille de la tournée pour
     un couple a b donné. Utilisé pour un graphe non complet

     @requires  ~verbose, l, a, b et acc quelconques
     @ensures   on renvoie l'inversion la plus bénéfique trouvée pour un couple
                donné
  *)
  let rec find_inversion_non_complet exist_fun ~verbose l a b acc  =
    match l with
    | [] -> b, acc
    | fin::[] -> b, acc@[fin]
    | c::d::q ->
      let name_a, name_b, name_c, name_d =
        N.get_name a,N.get_name b,N.get_name c,N.get_name d in
      if exist_fun name_a name_c &&
         exist_fun name_b name_d &&
         N.compare a c !=0 &&
         N.compare b d !=0
      then
        let road_diff = get_road_diff_inv a b c d in
        if road_diff < -0.001 then
          let _ =
            (if verbose then
               print_avant_apres_inv a b c d road_diff "OUI"
             else ()) in
          c, (List.rev acc)@(b::d::q)
        else
          find_inversion_non_complet exist_fun ~verbose (d::q) a b (acc@[c])
      else
        find_inversion_non_complet exist_fun ~verbose (d::q) a b (acc@[c])

  (* [find_inversion_non_complet exist_fun ~verbose l a b acc]
     fonction qui recherche et applique les inversions qui diminuent la taille
     de la tournée.

     @requires  road_list contient au moins une fois toutes les villes de la
                tournée, cities_list contient une fois toute les villes de la
                tournée, autres paramètres quelconques
     @ensures   la nouvelle route est contenue dans road_list, elle est de distance
                inférieure à celle de road_list en entrée de fonction
  *)
  let rec inversion_locale_aux road_list cities_list test count changed_nodes
      find_inversion_fun ~graphic ~verbose =
    let inversion_locale_aux_interne road_list test count changed_nodes =
      match road_list with
      | a::b::q ->
        let new_b, new_partial_liste = find_inversion_fun q a b []  in
        let new_liste = new_b::new_partial_liste@[a] in
        if N.compare new_b b != 0 then
          let changed_nodes = b::new_b::changed_nodes in
          (if verbose then print_road new_liste else ())
        ; (if graphic then
             let _ = print_tournee_with_modif road_list cities_list changed_nodes in
             Unix.sleep 1)
        ; inversion_locale_aux new_liste cities_list true (count-1)
            changed_nodes find_inversion_fun ~graphic ~verbose
        else
          inversion_locale_aux new_liste cities_list test (count-1)
            changed_nodes find_inversion_fun ~graphic ~verbose
      | _ -> road_list
    in
    if count <= 0 then
      if not test then road_list
      else
        let count = List.length road_list in
        inversion_locale_aux_interne road_list false count changed_nodes
    else inversion_locale_aux_interne road_list test count changed_nodes

  (* [repositionnement road_list cities_list tournee complet ~verbose ~graphic]
     fonction qui appelle la fonction de repositionnement adaptée au type de graphe,
     indiqué par la variable complet.

     @requires  road_list contient au moins une fois toutes les villes de la
                tournée, cities_list contient une fois toute les villes de la
                tournée, autres paramètres quelconques
     @ensures   la nouvelle route renvoyée est de distance
                inférieure à celle de road_list en entrée de fonction
  *)
  let repositionnement road_list cities_list tournee complet ~verbose ~graphic =
    let test = false in
    let count = List.length road_list in
    if complet then
      repositionnement_aux_complet road_list cities_list test count []
        ~verbose ~graphic
    else
      let exist_fun = CityGraph.exists tournee in
      repositionnement_aux_non_complet road_list cities_list test count []
        exist_fun ~verbose ~graphic

  (* [inversion_locale road_list cities_list tournee complet ~verbose ~graphic]
     fonction qui appelle la fonction d'inversion adaptée au type de graphe,
     indiqué par la variable complet.

     @requires  road_list contient au moins une fois toutes les villes de la
                tournée, cities_list contient une fois toute les villes de la
                tournée, autres paramètres quelconques
     @ensures   la nouvelle route renvoyée est de distance
                inférieure à celle de road_list en entrée de fonction
  *)
  let inversion_locale road_list cities_list tournee complet ~verbose ~graphic =
    let test = false in
    let count = List.length road_list in
    if complet then inversion_locale_aux road_list cities_list test count []
        (find_inversion_complet ~verbose) ~verbose ~graphic
    else
      let exist_fun = CityGraph.exists tournee in
      inversion_locale_aux road_list cities_list test count []
        (find_inversion_non_complet exist_fun ~verbose) ~verbose ~graphic

  (* [optimisation road_list cities_list tournee complet third_step rognage
      ~verbose ~graphic]
     fonction qui appelle la fonction d'optimisation précisée dans third_step.

     @requires  road_list contient au moins une fois toutes les villes de la
                tournée, cities_list contient une fois toute les villes de la
                tournée, autres paramètres quelconques
     @ensures   la nouvelle route renvoyée est de distance
                inférieure à celle de road_list en entrée de fonction
  *)
  let optimisation road_list cities_list tournee complet third_step rognage
      ~verbose ~graphic =
    let road_list =
      if rognage && not complet
      then List.tl road_list
      else road_list
    in
    let opti_fun = match third_step with
      | REPOSITIONNEMENT ->
        (if verbose then printf "REPOSITIONNEMENT :\n" else ())
      ; repositionnement
      | INVERSION        ->
        (if verbose then printf "INVERSION :\n" else ())
      ; inversion_locale
    in
    let road_list =
      opti_fun road_list cities_list tournee complet ~verbose ~graphic
    in
    (if graphic then
       let _ = print_tournee road_list cities_list in Unix.sleep 1)
  ; (if verbose then
       let _ = print_simple_road road_list in
       printf "Distance totale : %f\n" (get_full_distance road_list)
     else ())
  ; road_list

  (* [get_input_prameters ()]
     fonction qui lit sur l'entrée du programme et regarde si des flags ont été
     précisés par l'utilisateur. Le flag -v (verbose) donnera plus d'informations
     textuelles lors de l’exécution du programme, et -g (graphic) donne une
     représentation graphique du problème.

     @requires  aucun paramètre
     @ensures   retourne deux booléens
  *)
  let get_input_prameters () =
    let l = Sys.argv in
    let len = Array.length l in
    if len = 4 then
      let verbose = l.(1) = "-v" || l.(2) = "-v" || l.(3) = "-v" in
      let graphic = l.(1) = "-g" || l.(2) = "-g" || l.(3) = "-g" in
      verbose, graphic
    else if len = 3 then
      let verbose = l.(1) = "-v" || l.(2) = "-v" in
      let graphic = l.(1) = "-g" || l.(2) = "-g" in
      verbose, graphic
    else if len = 2 then
      let verbose = l.(1) = "-v" in
      let graphic = l.(1) = "-g" in
      verbose, graphic
    else false, false

  (* [get_run ()]
     fonction qui lit sur l'entrée du programme et regarde si l'on souhaite lancer
     le programme sur les fichiers param.txt et ville.txt et ainsi obtenir un résultat

     @requires  aucun paramètre
     @ensures   retourne un booléen
  *)
  let get_run () =
    let l = Sys.argv in
    let len = Array.length l in
    if len = 4 then
      l.(1) = "-r" || l.(2) = "-r" || l.(3) = "-r"
    else if len = 3 then
      l.(1) = "-r" || l.(2) = "-r"
    else if len = 2 then
      l.(1) = "-r"
    else false

  (* [get_parameters ~verbose ()]
     fonction qui lit sur dans le fichier de paramètres les paramètres précisés
     par l'utilisateur.

     @requires  verbose quelconque
     @ensures   retourne les paramètres
  *)
  let get_parameters ~verbose ~path_from_root_param () =
    let get_first_step e = match e with
      | "ONE" as p1->
        (if verbose then printf "First step parameter : %s\n" p1 else ())
      ; ONE
      | "HULL" as p2 ->
        (if verbose then printf "First step parameter : %s\n" p2 else ())
      ; HULL
      | s -> raise (Wrong_parameters (s^" : Unknown first step"))
    in
    let get_second_step e = match e with
      | "RANDOM" as p1 ->
        (if verbose then printf "Second step parameter : %s\n" p1 else ())
      ; RANDOM
      | "NEAREST" as p2 ->
        (if verbose then printf "Second step parameter : %s\n" p2 else ())
      ; NEAREST
      | "FARTHEST" as p3 ->
        (if verbose then printf "Second step parameter : %s\n" p3 else ())
      ; FARTHEST
      | s -> raise (Wrong_parameters (s^" : Unknown second step"))
    in
    let get_third_step e = match e with
      | "REPOSITIONNEMENT" as p1 ->
        (if verbose then printf "Third step parameter : %s\n" p1 else ())
      ; REPOSITIONNEMENT
      | "INVERSION" as p2 ->
        (if verbose then printf "Third step parameter : %s\n" p2 else ())
      ; INVERSION
      | s -> raise (Wrong_parameters (s^" : Unknown third step"))
    in
    let step_1, step_2, step_3 = Reader.get_params_from_file path_from_root_param in
    get_first_step step_1, get_second_step step_2, get_third_step step_3


  (* [get_cities ?complet:(complet=true) ~verbose ()]
     fonction qui lit sur dans le fichier de villes les villes ainsi que leurs
     caractéristiques

     @requires  complet par défaut est à vrai, ~verbose quelconque
     @ensures   retourne deux listes, une pour les villes, l'autre pour les routes
  *)
  let get_cities ?complet:(complet=true) ~verbose ~path_from_root_city () =
    Reader.get_infos_from_file complet path_from_root_city ~verbose

  (* [set_kd_tree city_list ~verbose ~graphic]
     fonction qui applique un pré traitement sur les villes en les placant dans
     un arbre kd et en renvoyant cet arbre.

     @requires  city_list et verbose quelconque
     @ensures   retourne l'arbre kd
  *)
  let set_kd_tree city_list ~verbose ~graphic =
    (if verbose then printf "Set kd tree : \n" else ())
  ; let kd = KDTree.creation city_list false in
    (if verbose then KDTree.print_kd_tree kd else ())
  ; kd

  (* [add_cities_to_tournee city_and_road_list]
     fonction qui ajoute les routes au graphe de tournée

     @requires  city_and_road_list quelconque
     @ensures   retourne le graphe de la tournée
  *)
  let add_cities_to_tournee city_and_road_list =
    Some (CityGraph.construct_graph city_and_road_list)


  (* [process_step_1
      ?tournee:(tournee=CityGraph.empty)
      ?complet:(complet=true)
      ~first_step
      ~verbose
      ~graphic
      cities_list
      kd_tree]
     fonction qui réalise l'étape 1 du projet, pour la partie graphe complet ou
     non complet

     @requires  tournee est par défaut vide, complet vaut par défaut vrai, les
                autres paramètres sont quelconques
     @ensures   retourne la liste des routes, la liste des villes non présentes
                dans la tournée, l'arbre k2 et un booléen indiquant si la tournée
                n'est composée que d'une ville
  *)
  let process_step_1
      ?tournee:(tournee=CityGraph.empty)
      ?complet:(complet=true)
      ~first_step
      ~verbose
      ~graphic
      cities_list
      kd_tree =
    let road_list, city_set =
      (if verbose then printf ("FIRST STEP: \n") else ())
    ; match first_step with
    | ONE ->
      let first_city = get_a_city cities_list in
      (if verbose then N.print_name (List.hd (fst(first_city))) else ())
    ; first_city
    | HULL ->
      let road_list = get_convex_hull cities_list in
      (if verbose then print_simple_road road_list else ())
    ; (if graphic then print_tournee road_list cities_list else ())
    ; let city_set  = get_city_set cities_list road_list in
      road_list, city_set
    in
    let kd_tree = add_road_list_on_kd_tree road_list kd_tree in
    road_list, city_set, kd_tree, List.length road_list = 1

  (* process_step_2
      ?tournee:(tournee=CityGraph.empty)
      ?complet:(complet=true)
      ~second_step
      ~verbose
      ~graphic
      city_set
      road_list
      cities_list
      kd_tree]
     fonction qui réalise l'étape 2 du projet, pour la partie graphe complet ou
     non complet

     @requires  tournee est par défaut vide, complet vaut par défaut vrai, city_set
                et road_list sont disjoints, cities_list et kd_tree contiennent toutes le
                villes.
     @ensures   retourne la route la plus optimale trouvée
  *)
  let process_step_2
      ?tournee:(tournee=CityGraph.empty)
      ?complet:(complet=true)
      ~second_step
      ~verbose
      ~graphic
      city_set
      road_list
      cities_list
      kd_tree =
    insert_all_cities city_set road_list cities_list kd_tree tournee complet
      second_step ~verbose ~graphic

  (* process_step_3
      ?tournee:(tournee=CityGraph.empty)
      ?complet:(complet=true)
      ~third_step
      ~verbose
      ~graphic
      road_list
      cities_list
      rognage]
     fonction qui réalise l'étape 3 du projet, pour la partie graphe complet ou
     non complet

     @requires  tournee est par défaut vide, complet vaut par défaut vrai,
                cities_list est contenue dans road_list, les autres paramètres
                sont quelconques
     @ensures   retourne la route la plus optimale trouvée
  *)
  let process_step_3
      ?tournee:(tournee=CityGraph.empty)
      ?complet:(complet=true)
      ~third_step
      ~verbose
      ~graphic
      road_list
      cities_list
      rognage =
    optimisation road_list cities_list tournee complet third_step rognage
      ~verbose ~graphic


end