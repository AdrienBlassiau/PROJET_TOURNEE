(**************************************************************************)
(*                                                                        *)
(*                  Adrien Blassiau, projet IPF 2018-2019                 *)
(*                                                                        *)
(*      Tests d'un module de villes composés de constructeurs, getters,   *)
(*      setters et d'opérations utiles sur les villes                     *)
(*                                                                        *)
(**************************************************************************)

open Test
open Ville

let tests_ville_bool =
  [ "test_construct_and_deconstruct_ville",
    (fun () ->
       deconstruct_ville @@ construct_ville ("a",1.,2.) = ("a",1.,2.)),
    true, fprintf_bool;
    "test_get_name_ville",
    (fun () ->
       get_name @@ construct_ville ("a",1.,2.) = "a"),
    true, fprintf_bool;
    "test_get_x_ville",
    (fun () ->
       get_x @@ construct_ville ("a",1.,2.) = 1.),
    true, fprintf_bool;
    "test_get_y_ville",
    (fun () ->
       get_y @@ construct_ville ("a",1.,2.) = 2.),
    true, fprintf_bool;
    "test_set_name_ville",
    (fun () ->
       deconstruct_ville @@ set_name (construct_ville ("a",1.,2.)) "b" = ("b",1.,2.)),
    true, fprintf_bool;
    "test_set_x_ville",
    (fun () ->
       deconstruct_ville @@ set_x (construct_ville ("a",1.,2.)) 3. = ("a",3.,2.)),
    true, fprintf_bool;
    "test_set_y_ville",
    (fun () ->
       deconstruct_ville @@ set_y (construct_ville ("a",1.,2.)) 4. = ("a",1.,4.)),
    true, fprintf_bool;
    "test_compare_ville",
    (fun () ->
       compare_name (construct_ville ("a",1.,2.)) (construct_ville ("b",1.,2.)) < 0),
    true, fprintf_bool;
    "test_compare_x_ville",
    (fun () ->
       compare_x (construct_ville ("a",1.,2.)) (construct_ville ("a",3.,2.)) < 0),
    true, fprintf_bool;
    "test_compare_y_ville",
    (fun () ->
       compare_y (construct_ville ("a",1.,2.)) (construct_ville ("a",1.,4.)) < 0),
    true, fprintf_bool;
    "get_angle",
    (fun () ->
       1.57 -. get_angle (construct_ville ("a",1.,2.)) (construct_ville ("a",1.,4.)) < 0.001 ),
    true, fprintf_bool;
    "get_dist",
    (fun () ->
       1.57 -. get_dist (construct_ville ("a",1.,2.)) (construct_ville ("a",1.,4.)) -. 2. < 0.001 ),
    true, fprintf_bool;
    "get_dist_lazy",
    (fun () ->
       1.57 -. get_dist_lazy (construct_ville ("a",1.,2.)) (construct_ville ("a",1.,4.)) -. 4. < 0.001 ),
    true, fprintf_bool;
    "get_tournant_droite",
    (fun () ->
       get_tournant (construct_ville ("a",1.,2.)) (construct_ville ("a",2.,3.)) (construct_ville ("a",3.,5.)) > 0. ),
    true, fprintf_bool;
    "get_tournant_gauche",
    (fun () ->
       get_tournant (construct_ville ("a",1.,2.)) (construct_ville ("a",2.,3.)) (construct_ville ("a",3.,-1.)) < 0. ),
    true, fprintf_bool;
  ]