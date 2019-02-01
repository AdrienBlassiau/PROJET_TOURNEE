(**************************************************************************)
(*                                                                        *)
(*                  Adrien Blassiau, projet IPF 2018-2019                 *)
(*                                                                        *)
(*                  Tests des fonctions du module useful                  *)
(*                                                                        *)
(**************************************************************************)

open Useful
open Test

let tests_useful_bool =
  [ "hd_and_tail_non_empty",
    (fun () ->
       hd_and_tail [1] = (1,[])),
    true, fprintf_bool;
    "hd_and_tail_non_empty_2",
    (fun () ->
       hd_and_tail ["1";"2";"3"] = ("1",["2";"3"])),
    true, fprintf_bool;
    "is_some_on_some",
    (fun () ->
       is_some @@ Some 3 = true ),
    true, fprintf_bool;
    "is_some_on_none",
    (fun () ->
       is_some None = false),
    true, fprintf_bool;
    "is_none_on_some",
    (fun () ->
       is_none @@ Some 3 = false ),
    true, fprintf_bool;
    "is_none_on_none",
    (fun () ->
       is_none None = true),
    true, fprintf_bool;
    "get_some_on_some",
    (fun () ->
       get_some @@ Some "toto" = "toto"),
    true, fprintf_bool;
    "norme_carre_pos",
    (fun () ->
       norme_carre (1.,2.) (3.,4.) = 8.),
    true, fprintf_bool;
    "norme_carre_neg",
    (fun () ->
       norme_carre (-1.,-2.) (-3.,-4.) = 8.),
    true, fprintf_bool;
    "norme_on_0",
    (fun () ->
       norme (0.,0.) (0.,0.) = 0.),
    true, fprintf_bool;
    "norme",
    (fun () ->
       norme (1.,2.) (3.,4.) ** 2. -. 8. < 0.00001),
    true, fprintf_bool;
    "split_empty",
    (fun () ->
       split [] = ([],None,[])),
    true, fprintf_bool;
    "split_sorted_list",
    (fun () ->
       split [1;2;3;4;5] = ([1;2],Some 3,[4;5])),
    true, fprintf_bool;
    "split_not_sorted_list",
    (fun () ->
       split [2;3;1;0;5] = ([2;3],Some 1,[0;5])),
    true, fprintf_bool;
    "test_shuffle_empty",
    (fun () ->
       shuffle [] = []),
    true, fprintf_bool;
    "test_shuffle_not_empty",
    (fun () ->
       List.fast_sort compare (shuffle [2;3;1;4;5]) = [1;2;3;4;5]),
    true, fprintf_bool;]


let tests_useful_exn  =
  [ "hd_and_tail_empty",
    (fun () ->
       try
         ignore @@ hd_and_tail [];
         raise @@ Error ("Should raise",EmptyList)
       with e -> e),
    EmptyList, fprintf_exn;
    "get_some_on_none",
    (fun () ->
       try
         ignore @@ get_some None;
         raise @@ Error ("Should raise",IsNone)
       with e -> e),
    IsNone, fprintf_exn;
  ]