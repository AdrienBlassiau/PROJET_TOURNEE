(**************************************************************************)
(*                                                                        *)
(*                  Adrien Blassiau, projet IPF 2018-2019                 *)
(*        Corps du main utilisé pour lancer l'algorithme de               *)
(*        recherche d'une tournée optimale sur un graphe non complet      *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Graph
open Read
open Tournee

module Tournee = MakeTournee(Ville.OrderedTypeVille)

let _ = Random.self_init ()

(* [main_graphe_non_complet ~path_from_root_param ~path_from_root_city]
   est la fonction principale de la partie 1 du projet qui prend deux fichiers :
   path_from_root_param pour les paramètres et path_from_root_city pour les villes
   et mes routes, et retourne la tournée trouvée

   @requires  les fichiers existent et on fournit leur chemin depuis
              la racine du projet
   @ensures   le résultat est une liste
*)
let main_graphe_non_complet ~path_from_root_param ~path_from_root_city =
  let tini = Sys.time () in
  let complet = Some false in
  let verbose, graphic         = Tournee.get_input_prameters () in
  let first_step,
      second_step,
      third_step               =
    Tournee.get_parameters ~verbose ~path_from_root_param ()
  in
  let cities_list, roads_list  =
    Tournee.get_cities ?complet ~verbose ~path_from_root_city ()
  in
  let kd_tree           = Tournee.set_kd_tree cities_list ~verbose ~graphic in
  let tournee           = Tournee.add_cities_to_tournee roads_list in


  (*##########################################################################*)
  (*#************************************************************************#*)
  (*#*******************************  STEP 1  *******************************#*)
  (*#************************************************************************#*)
  (*##########################################################################*)
  let t0 = Sys.time () in

  let road_list, city_set, kd_tree, rognage =
    Tournee.process_step_1
      ?tournee
      ?complet
      ~first_step
      ~verbose
      ~graphic
      cities_list
      kd_tree
  in

  let t1 = Sys.time () in
  (if verbose then
     print_endline ("Le temps d'execution de l'étape 1 est de :"^(string_of_float (t1-.t0)))
   else ());

  (*##########################################################################*)
  (*#************************************************************************#*)
  (*#*******************************  STEP 2  *******************************#*)
  (*#************************************************************************#*)
  (*##########################################################################*)

  let t0 = Sys.time () in


  let road_list =
    Tournee.process_step_2
      ?tournee
      ?complet
      ~second_step
      ~verbose
      ~graphic
      city_set
      road_list
      cities_list
      kd_tree
  in

  let t1 = Sys.time () in
  (if verbose then
     print_endline ("Le temps d'execution de l'étape 2 est de :"^(string_of_float (t1-.t0)))
   else ());

  (*##########################################################################*)
  (*#************************************************************************#*)
  (*#*******************************  STEP 3  *******************************#*)
  (*#************************************************************************#*)
  (*##########################################################################*)

  let t0 = Sys.time () in

  let road_list =
    if List.length road_list > 2 then
      Tournee.process_step_3
        ?tournee
        ?complet
        ~third_step
        ~verbose
        ~graphic
        road_list
        cities_list
        rognage
    else road_list
  in

  let t1 = Sys.time () in
  (if verbose then
     print_endline ("Le temps d'execution de l'étape 3 est de :"^(string_of_float (t1-.t0)))
   else ());

  let tfin = Sys.time () in

  (if verbose then
     print_endline ("Le temps d'execution total est de :"^(string_of_float (tfin-.tini)))
   else ());

  road_list



let _ =
  if Tournee.get_run () then
    let road_list = main_graphe_non_complet
        ~path_from_root_param:"param.txt"
        ~path_from_root_city:"ville.txt"
    in
    Tournee.print_simple_road road_list
  else ()