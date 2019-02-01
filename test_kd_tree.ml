(**************************************************************************)
(*                                                                        *)
(*                  Adrien Blassiau, projet IPF 2018-2019                 *)
(*                                                                        *)
(*      Tests d'un module d'arbre 2-d sur des points flottants.           *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Test
open Kd_tree
open Graph

module MPts = struct
  type t = string * float * float

  let compare (n1,x1,y1) (n2,x2,y2) = compare n1 n2
  let compare_x (n1,x1,y1) (n2,x2,y2) = Pervasives.compare x1 x2
  let compare_y (n1,x1,y1) (n2,x2,y2) = Pervasives.compare y1 y2
  let print_name (n1,x1,y1) = print_string n1
  let get_name (n1,x1,y1) = n1
  let get_infos (n1,x1,y1) = (n1,x1,y1)
  let get_dist_lazy (n1,x1,y1) (n2,x2,y2) = Useful.norme_carre (x1,y1) (x2,y2)

end

module T2dtree = MakeKDTree(MPts)

open T2dtree

let l_pts = generate_random_points 10000

let pts = ("10001",42.2,88.4)

let kdt = creation l_pts true

let find_nearest_neighbour_v1 pts l_pts =
  	find_nearest_neighbour_complet pts kdt

let find_nearest_neighbour_v2 pts l_pts =
  List.fold_left (fun (best_pts,best_dist) current ->
      let _,x1,y1 = pts in
      let _,x2,y2 = current in
      let dist = Useful.norme_carre (x1,y1) (x2,y2) in
      if dist < best_dist || best_dist < 0. then current,dist
      else  best_pts,best_dist ) (pts,(-1.)) l_pts


let res1 = test_with find_nearest_neighbour_v1 pts l_pts

let res2 = test_with find_nearest_neighbour_v2 pts l_pts


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
let l_pts_2 = [("1",4.,0.);("2",9.,0.);("3",1.,3.);("4",7.,3.);("5",15.,3.);("6",19.,3.);("7",8.,5.);("8",20.,6.);("9",5.,7.);("10",9.,8.);("11",16.,8.)]

let roads = [("1",["2";"3";"4"]);("2",["1"]);("3",["1"]);("4",["1"]);("5",[]);("6",[]);("7",[]);("8",[]);("9",[]);("10",[]);("11",[])]


let kd_tree_true = creation l_pts_2 true
let kd_tree_false = creation l_pts_2 false

let tests_kd_tree_bool =
  [ "is_empty_is_empty",
    (fun () ->
       is_empty empty),
    true, fprintf_bool;
    "find_existing_node_and_accessible",
    (fun () ->
       fst @@ find_nearest_neighbour_complet ("5",15.,3.) kd_tree_true = ("6",19.,3.)),
    true, fprintf_bool;
    "find_existing_node_and_not_accessible",
    (fun () ->
       fst @@ find_nearest_neighbour_complet ("5",15.,3.) kd_tree_false = ("5",15.,3.)),
    true, fprintf_bool;
    "find_existing_node_and_not_accessible",
    (fun () ->
       fst @@ find_nearest_neighbour_complet ("5",15.,3.) kd_tree_false = ("5",15.,3.)),
    true, fprintf_bool;
    "find_existing_node_and_change_status_of_1_node",
    (fun () ->
       fst @@ find_nearest_neighbour_complet ("5",15.,3.) @@ change_status kd_tree_false ("1",4.,0.) = ("1",4.,0.)),
    true, fprintf_bool;
    "find_existing_node_and_change_status_of_2_node",
    (fun () ->
       fst @@ find_nearest_neighbour_complet ("5",15.,3.) @@ change_status (change_status kd_tree_false ("1",4.,0.)) ("4",7.,3.) = ("4",7.,3.)),
    true, fprintf_bool;]

let tests_kd_tree_exn  =
  [ "creation_on_empty_list",
    (fun () ->
       try
         ignore @@ creation [] true;
         raise @@ Error ("Should raise",Impossible)
       with e -> e),
    Impossible, fprintf_exn;
    "change_status_not_existing_point",
    (fun () ->
       try
         ignore @@ change_status kd_tree_true ("21",2.,3.);
         raise @@ Error ("Should raise",Not_found)
       with e -> e),
    Not_found, fprintf_exn;
  ]

