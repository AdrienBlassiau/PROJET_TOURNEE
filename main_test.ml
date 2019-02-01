(**************************************************************************)
(*                                                                        *)
(*                  Adrien Blassiau, projet IPF 2018-2019                 *)
(*        Corps du main utilisé pour lancer les test unitaires des        *)
(*        fonctions du projets                                            *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Printf
open Test
open Test_graph
open Test_useful
open Test_convex_hull
open Test_my_stack
open Test_kd_tree
open Test_city_set
open Test_read
open Test_ville

let main () =
  printf "\027[31mTEST GRAPHE\027[0m\n";
  List.iter do_test tests_graph_bool;
  List.iter do_test tests_graph_exn;
  printf "\n\027[31mTEST USEFUL\027[0m\n";
  List.iter do_test tests_useful_bool;
  List.iter do_test tests_useful_exn;
  printf "\n\027[31mTEST CONVEX HULL\027[0m\n";
  List.iter do_test tests_convex_hull_bool;
  List.iter do_test tests_convex_hull_exn;
  printf "\n\027[31mTEST STACK\027[0m\n";
  List.iter do_test tests_stack_bool;
  List.iter do_test tests_stack_exn;
  printf "\n\027[31mTEST 2-D TREE\027[0m\n";
  List.iter do_test tests_kd_tree_bool;
  List.iter do_test tests_kd_tree_exn;
  Format.printf "Plus proche voisin de 42.2/88.4 sur un ensemble de 10000 points générés aléatoirement \n";
  Format.printf
    "Arbre 2-d : %f s et %a\n"
    (fst res1) fprintf_pts (fst @@ snd res1);
  Format.printf
    "Naïf      : %f s et %a\n"
    (fst res2) fprintf_pts (fst @@ snd res2);
  printf "\n\027[31mTEST CITY SET\027[0m\n";
  List.iter do_test tests_city_set_bool;
  printf "\n\027[31mTEST READ\027[0m\n";
  List.iter do_test tests_read_bool;
  List.iter do_test tests_read_exn;
  printf "\n\027[31mTEST VILLE\027[0m\n";
  List.iter do_test tests_ville_bool

let _ = main ()