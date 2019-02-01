(**************************************************************************)
(*                                                                        *)
(*                  Adrien Blassiau, projet IPF 2018-2019                 *)
(*                                                                        *)
(*      Tests d'un module de graphe d'entiers construit à partir          *)
(*      du foncteur de graphe                                             *)
(*                                                                        *)
(**************************************************************************)

open Test
open Graph

module MInt = struct
  type t = int
  let compare = compare
  let print s = print_int s
end

module Tgraphe= MakeGraph(MInt)
open Tgraphe

let tests_graph_bool =
  [ "empty_is_empty",
    (fun () ->
       is_empty empty),
    true, fprintf_bool;
    "not_empty_is_not_empty",
    (fun () -> is_empty @@ add_node 1 empty),
    false, fprintf_bool;
    "exist_in_empty_graph",
    (fun () -> exists empty 1 1),
    false, fprintf_bool;
    "succs_existing",
    (fun () -> NodeSet.is_empty @@ succs 1 @@ add_edge 1 2 empty),
    false, fprintf_bool;
    "exist_existing_edge",
    (fun () -> exists (add_edge 1 1 empty) 1 1),
    true, fprintf_bool;
    "exist_not_existing_edge",
    (fun () -> exists (add_edge 1 2 empty) 1 1),
    false, fprintf_bool;
    "construct_empty_graph",
    (fun () -> is_empty @@ construct_graph []),
    true, fprintf_bool;
    "construct_graph",
    (fun () -> count_edge @@ remove_edge 1 2 @@ remove_edge 1 3 @@ remove_edge 1 4 @@ remove_edge 2 1 @@ remove_edge 2 3 @@ remove_edge 2 4 @@ remove_edge 3 1 @@ remove_edge 3 2 @@ remove_edge 3 4 @@ remove_edge 4 1 @@ remove_edge 4 2 @@ remove_edge 4 3 @@ construct_graph [(1,[2;3;4]);(2,[1;3;4]);(3,[1;2;4]);(4,[1;2;3])] = 0),
    true, fprintf_bool;
    "add_and_remove_same_node",
    (fun () -> is_empty @@ remove_node 1 @@ add_node 1 empty),
    true, fprintf_bool;
    "add_and_remove_diff_node",
    (fun () -> is_empty @@ remove_node 2 @@ add_node 1 empty),
    false, fprintf_bool;
    "remove_node_empty_graph",
    (fun () -> is_empty @@ remove_node 2 @@ empty),
    true, fprintf_bool;
    "add_new_edge_empty_graph",
    (fun () -> is_empty @@ add_edge 2 2 @@ empty),
    false, fprintf_bool;
    "add_not_new_edge_empty_graph",
    (fun () -> is_empty @@ add_edge 2 2 @@ add_edge 2 2 @@ empty),
    false, fprintf_bool;
    "remove_existing_edge_empty_graph",
    (fun () -> is_empty @@ remove_edge 2 2 @@ add_edge 2 2 @@ empty),
    false, fprintf_bool;
    "remove_not_existing_edge_empty_graph",
    (fun () -> is_empty @@ remove_edge 2 1 @@ add_edge 2 2 @@ empty),
    false, fprintf_bool;
    "remove_existing_edge_and_node",
    (fun () -> is_empty @@ remove_node 2 @@ remove_edge 2 2 @@ add_edge 2 2 @@ empty),
    true, fprintf_bool;
    "fold_node_somme",
    (fun () -> fold_node (+) (add_node 1 @@ add_node 5 @@ add_node 2 @@ add_node 1 @@ empty) 0 = 8),
    true, fprintf_bool;
    "fold_edge_somme",
    (fun () -> fold_edge (fun src dst acc -> src + dst + acc) (add_edge 4 2 @@ add_edge 1 2 @@ add_edge 1 4 @@ add_edge 1 5 (empty)) 0 = 20),
    true, fprintf_bool;
    "count_node_graph_empty",
    (fun () -> count_node empty = 0),
    true, fprintf_bool;
    "count_node_graph_filled",
    (fun () -> count_node @@ add_node 1 @@ add_node 2 @@ add_node 4 @@ add_node 5 empty = 4),
    true, fprintf_bool;
    "count_edge_graph_empty",
    (fun () -> count_edge empty = 0),
    true, fprintf_bool;
    "count_edge_graph_filled",
    (fun () -> count_edge @@ add_edge 4 2 @@ add_edge 1 2 @@ add_edge 1 4 @@ add_edge 1 5 @@ empty = 4),
    true, fprintf_bool;
  ]

let tests_graph_exn  =
  [ "succs_not_found",
    (fun () ->
       try
         ignore @@ succs 1 empty;
         raise @@ Error ("Should raise",Not_found)
       with e -> e),
    Not_found, fprintf_exn;
  ]


