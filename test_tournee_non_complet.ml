(**************************************************************************)
(*                                                                        *)
(*                  Adrien Blassiau, projet IPF 2018-2019                 *)
(*                                                                        *)
(*      Test d'un module de tournée de ville construit à partir d'un      *)
(*      foncteur de tournée, pour la partie 2                             *)
(*                                                                        *)
(**************************************************************************)

open Scanf
open Test
open Tournee
open Main_graph_non_complet

module Ttournee = MakeTournee(Ville.OrderedTypeVille)
open Ttournee



(* [get_accessible_cities n i] crée une chaîne de caractères utilisées ensuite
   dans la construction du fichier avec les routes pour la partie 2 du projet

    @requires  n et i quelconque
    @ensures   le résultat est une chaîne de caractère
*)
let rec get_accessible_cities n i =
  if i > 0
  then (get_accessible_cities n (i-1))^(if i != n then " "^(string_of_int i) else "")
  else ""

(* [print_city oc i] crée une chaîne de caractères correspondant au nom
   de la ville, suivies de ses coordonnées, tirées aléatoirement.

    @requires  i positif
    @ensures   le résultat est écrit dans oc
*)
let rec print_city oc i = match i with
  | 0 -> ()
  | n->
    print_city oc (n-1)
  ; Printf.fprintf oc "%s %f %f\n"
      (string_of_int n)
      (Random.float 1000.)
      (Random.float 1000.)

(* [print_road oc i tot] crée une chaîne de caractères correspondant au nom
   de la ville, suivies des villes adjacentes

    @requires  i positif
    @ensures   le résultat est écrit dans oc
*)
let rec print_road oc i tot = match i with
  | 0 -> ()
  | n-> let accessible_cities = get_accessible_cities n tot in
    print_road oc (n-1) tot
  ; Printf.fprintf oc "%s :%s\n" (string_of_int n) accessible_cities


let string_road (dist,_) = string_of_int (int_of_float dist)

(* [generate_files i] génère un nombre i de fichier comportant chacun un nombre
   aléatoire en 0 et 1000 de villes

    @requires  i positif
    @ensures   le résultat est écrit dans un fichier
*)
let rec generate_files_test i =
    let oc1 = open_out ("test_non_complet/files.txt") in
    let rec generate_files_test_aux i =
    if i < 0 then ()
    else
      let _ = print_endline ("Generate test n°"^(string_of_int i)) in
      let nb = (Random.int 100)+1 in
      let param_tab =["HFI";"HFR";"HNI";"HNR";"HRI";"HRR";"OFI";"OFR";"ONI";"ONR";"ORI";"ORR"] in
      let file_name = "test_"^(string_of_int i)^".txt" in
      let oc2 = open_out ("test_non_complet/"^file_name) in
      let _ = Printf.fprintf oc2 "%d\n" nb in
      let _ = print_city oc2 nb in
      let _ = print_road oc2 nb nb in
      let _ = close_out oc2  in
      let choice_param = List.nth param_tab (Random.int 12) in
      let param_file_name = "param"^choice_param^".txt" in
      let road_list = main_graphe_non_complet
          ~path_from_root_param:("test_non_complet/"^param_file_name)
          ~path_from_root_city:("test_non_complet/"^file_name)
      in
      let sol1 = string_road (Tournee.get_string_and_float_road road_list) in
      Printf.fprintf oc1 "%s %s %s\n" file_name param_file_name sol1;
      generate_files_test_aux (i-1)
  in generate_files_test_aux i


let rec check_test i =
    let oc1 = Scanning.from_channel (open_in ("test_non_complet/files.txt")) in
    let rec check_test_aux i oc1 =
    if i < 0 then ()
    else
      let file_name, param_file_name, solution =
        Scanf.bscanf oc1 "%s %s %[0-9: ] " (fun f p s -> f,p,s)
      in
      (* print_endline ("On trouve :"^file_name^" et "^param_file_name^" et "^solution); *)
      let road_list = main_graphe_non_complet
          ~path_from_root_param:("test_non_complet/"^param_file_name)
          ~path_from_root_city:("test_non_complet/"^file_name)
      in
      let sol = string_road (Tournee.get_string_and_float_road road_list) in
      if (String.get param_file_name 6) = 'R' then
        print_endline ("Test fichier :"^file_name^" -> "^sol^" (\027[32maléatoire donc pas vérifiable\027[0m)")
      else
        print_endline ("Test fichier :"^file_name^" -> "^sol^" = "^solution^" ? "^"\027[32m"^(string_of_bool (sol = solution))^"\027[0m");
      check_test_aux (i-1) oc1
  in check_test_aux i oc1


(* let _ = generate_files_test 250 *)
let _ = check_test 250