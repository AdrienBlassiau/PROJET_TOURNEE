(**************************************************************************)
(*                                                                        *)
(*                  Adrien Blassiau, projet IPF 2018-2019                 *)
(*                                                                        *)
(*      Test d'un module utilisé pour la construction d'une enveloppe     *)
(*      convexe en dimension 2 avec des points flottants du plan          *)
(*                                                                        *)
(**************************************************************************)

open Test
open Convex_hull
open My_stack

module MPts = struct
  type t = float * float

  let get_angle pt1 pts2 = match pt1, pts2 with
    | (x1,y1) , (x2,y2) -> atan2 (y2 -. y1) (x2 -. x1)

  let get_tournant pts1 pts2 pts3 = match pts1,pts2,pts3 with
    | (x1,y1) , (x2,y2) , (x3,y3) ->
      (x2 -. x1)*.(y3 -. y1) -. (y2 -. y1)*.(x3 -. x1)

  let compare_x pts1 pts2 = match pts1,pts2 with
    | (x1,_) , (x2,_) -> compare x1 x2

  let compare_y pts1 pts2 = match pts1,pts2 with
    | (_,y1) , (_,y2) -> compare y1 y2

  let compare = compare

  let print (x,y) = print_float x ; print_float y

end

module Tconvexhull = MakeConvexHull(MPts)
open Tconvexhull

(********Situation********)
(*********x******x********)(*(9.,8.);(16.,8.)*)
(*****x*******************)(*(5.,7.)*)
(********************x****)(*(20.,6.)*)
(********x****************)(*(8.,5.)*)
(*************************)
(*x*****x*******x***x*****)(*(1.,3.);(7.,3.);(15.,3.);(19.,3.)*)
(*************************)
(*************************)
(****x****x***************)(*(4.,0.);(9.,0.)*)

let l_pts = [(4.,0.);(9.,0.);(1.,3.);(7.,3.);(15.,3.);(19.,3.);(8.,5.);(20.,6.);(5.,7.);(9.,8.);(16.,8.)]
let l_pts_without_pivot = [(9.,0.);(1.,3.);(7.,3.);(15.,3.);(19.,3.);(8.,5.);(20.,6.);(5.,7.);(9.,8.);(16.,8.)]
let l_pts_sorted = [(4.,0.);(9.,0.);(19.,3.);(15.,3.);(20.,6.);(16.,8.);(7.,3.);(8.,5.);(9.,8.);(5.,7.);(1.,3.)]
let convex_hull = [(4.,0.);(9.,0.);(19.,3.);(20.,6.);(16.,8.);(9.,8.);(5.,7.);(1.,3.)]

let tests_convex_hull_bool =
  [ "compare_graham_angle_different_x",
    (fun () ->
       compare_graham_angle (19.,3.) (20.,6.) (4.,0.) = -1),
    true, fprintf_bool;
    "get_pivot_test_non_empty",
    (fun () ->
       get_pivot l_pts = (4.,0.)),
    true, fprintf_bool;
    "remove_pivot_empty",
    (fun () ->
       remove_pivot [] (2.,0.) = []),
    true, fprintf_bool;
    "remove_not_present_pivot",
    (fun () ->
       remove_pivot l_pts (2.,0.) = l_pts),
    true, fprintf_bool;
    "remove_present_pivot",
    (fun () ->
       remove_pivot l_pts (4.,0.) = l_pts_without_pivot),
    true, fprintf_bool;
    "get_pivot_and_list_without_it",
    (fun () ->
       get_pts_coeff_list l_pts = (l_pts_without_pivot,(4.,0.))),
    true, fprintf_bool;
    "sort_pts_list_not_empty",
    (fun () ->
       sort_pts_list l_pts = l_pts_sorted),
    true, fprintf_bool;
    "manage_stack_empty",
    (fun () ->
       manage_stack new_stack (1.,1.) = new_stack),
    true, fprintf_bool;
    "manage_stack_non_empty",
    (fun () ->
       snd @@ pop @@ snd @@ pop @@ snd @@ pop @@ manage_stack (push (4.,0.) @@ push (9.,0.) @@ new_stack) (4.,0.) = new_stack),
    true, fprintf_bool;
    "get_convex_hull_empty",
    (fun () ->
       get_convex_hull [] = []),
    true, fprintf_bool;
    "get_convex_hull_one_element",
    (fun () ->
       get_convex_hull [(4.,0.)] = [(4.,0.)]),
    true, fprintf_bool;
    "get_convex_hull_two_element",
    (fun () ->
       get_convex_hull [(4.,0.);(9.,0.)] = [(9.,0.);(4.,0.)]),
    true, fprintf_bool;
    "get_convex_hull_three_element",
    (fun () ->
       get_convex_hull [(4.,0.);(9.,0.);(19.,3.)] = [(19.,3.);(9.,0.);(4.,0.)]),
    true, fprintf_bool;
    "get_convex_hull_four_element",
    (fun () ->
       get_convex_hull [(4.,0.);(9.,0.);(9.,8.);(8.,5.)] = [(9.,8.);(9.,0.);(4.,0.)]),
    true, fprintf_bool;
    "get_convex_hull_aligned",
    (fun () ->
       get_convex_hull [(1.,3.);(7.,3.);(15.,3.);(19.,3.)] = [(19.,3.);(1.,3.)]),
    true, fprintf_bool;
    "get_convex_hull_non_empty_11_elements",
    (fun () ->
       get_convex_hull l_pts = List.rev convex_hull),
    true, fprintf_bool;]


let tests_convex_hull_exn  =
  [ "get_pivot_test_empty",
    (fun () ->
       try
         ignore @@ get_pivot [];
         raise @@ Error ("Should raise",Failure "hd")
       with e -> e),
    Failure "hd", fprintf_exn;
    "get_pivot_and_list_without_it_empty_list",
    (fun () ->
       try
         ignore @@ get_pts_coeff_list [];
         raise @@ Error ("Should raise",Failure "hd")
       with e -> e),
    Failure "hd", fprintf_exn;
    "sort_pts_list_empty",
    (fun () ->
       try
         ignore @@ sort_pts_list [];
         raise @@ Error ("Should raise",Failure "hd")
       with e -> e),
    Failure "hd", fprintf_exn;
  ]