(**************************************************************************)
(*                                                                        *)
(*                  Adrien Blassiau, projet IPF 2018-2019                 *)
(*                                                                        *)
(*                  Test d'un module de Set de villes                     *)
(*                                                                        *)
(**************************************************************************)

open Test
open City_set

module MInt = struct
  type t = int
  let compare = compare
  let get_infos s = string_of_int s , float_of_int s , float_of_int s
end

module Tcityset= MakeCitySet(MInt)
open Tcityset

let tests_city_set_bool =
  [ "empty_is_empty",
    (fun () ->
       is_empty empty),
    true, fprintf_bool;
    "is_not_empty_is_not_empty",
    (fun () ->
       is_empty empty),
    true, fprintf_bool;
    "make_city_set_with_empty_list",
    (fun () ->
       make_city_set [] = empty),
    true, fprintf_bool;
    "make_city_set_with_not_empty_list",
    (fun () ->
       remove_city 2 @@ remove_city 1 @@ make_city_set [1;2] = empty),
    true, fprintf_bool;
    "city_set_to_list_empty",
    (fun () ->
       city_set_to_list empty = [] ),
    true, fprintf_bool;
    "city_set_to_list_not_empty",
    (fun () ->
       city_set_to_list @@ make_city_set [1;2] = [1;2]),
    true, fprintf_bool;
    "remove_city_on_empty",
    (fun () ->
       remove_city 1 empty = empty),
    true, fprintf_bool;
    "remove_city_existing",
    (fun () ->
       remove_city 1 @@ make_city_set [1;2] = make_city_set [2]),
    true, fprintf_bool;
    "remove_city_not_existing",
    (fun () ->
       remove_city 3 @@ make_city_set [1;2] = make_city_set [1;2]),
    true, fprintf_bool;
    "remove_cities_on_empty",
    (fun () ->
       remove_cities [1;2] empty = empty),
    true, fprintf_bool;
    "remove_cities_existing",
    (fun () ->
       remove_cities [1;2] @@ make_city_set [1;2] = empty),
    true, fprintf_bool;
    "remove_cities_not_existing",
    (fun () ->
       remove_cities [4;5] @@ make_city_set [1;2] = make_city_set [1;2]),
    true, fprintf_bool;
    "fold_plus",
    (fun () ->
       fold (+) (make_city_set [1;2;3;4;5]) 0 = 15),
    true, fprintf_bool;
    "get_size_empty",
    (fun () ->
       get_size @@ make_city_set [] = 0),
    true, fprintf_bool;
    "get_size_not_empty",
    (fun () ->
       get_size @@ make_city_set [1;2;3;4;5] = 5),
    true, fprintf_bool;
    "get_city_on_empty",
    (fun () ->
       get_city empty = None),
    true, fprintf_bool;
    "get_city_on_not_empty",
    (fun () ->
       get_city @@ make_city_set [1] = Some 1),
    true, fprintf_bool;
  ]
