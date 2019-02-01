(**************************************************************************)
(*                                                                        *)
(*                  Adrien Blassiau, projet IPF 2018-2019                 *)
(*                                                                        *)
(*      Tests des fonctions liées à la construction d'une pile (FILO).    *)
(*                                                                        *)
(**************************************************************************)

open Test
open My_stack

let tests_stack_bool =
  [ "empty_stack_size_0",
    (fun () ->
       get_size new_stack = 0),
    true, fprintf_bool;
    "filled_stack_size",
    (fun () ->
       get_size @@ push 12 @@ push 23 @@ push 4 @@ push 3 new_stack = 4),
    true, fprintf_bool;
    "push_and_pop_on_empty_stack",
    (fun () ->
       pop @@ snd @@ pop @@ snd @@ pop @@ snd @@ pop @@ snd @@ pop @@ push 12 @@
       push 23 @@ push 4 @@ push 3 new_stack = (None,new_stack)),
    true, fprintf_bool;
    "get_first_not_empty",
    (fun () ->
       get_first @@ push 4 @@ push 3 new_stack = 4),
    true, fprintf_bool;
    "get_first_and_second_size_sup_2",
    (fun () ->
       get_first_and_second @@ push 4 @@ push 3 new_stack = (4,3)),
    true, fprintf_bool;
  "fold_sum_element",
    (fun () ->
       fold (+) (push 1 @@ push 4 @@ push 3 new_stack) 0 = 8),
    true, fprintf_bool;]



let tests_stack_exn  =
  [ "get_first_empty",
    (fun () ->
       try
         ignore @@ get_first new_stack;
         raise @@ Error ("Should raise",TooShort)
       with e -> e),
    TooShort, fprintf_exn;
    "get_first_and_second_size_1",
    (fun () ->
       try
         ignore @@ get_first_and_second @@ push 3 new_stack;
         raise @@ Error ("Should raise",TooShort)
       with e -> e),
    TooShort, fprintf_exn;
    "get_first_and_second_empty",
    (fun () ->
       try
         ignore @@ get_first_and_second @@ new_stack;
         raise @@ Error ("Should raise",TooShort)
       with e -> e),
    TooShort, fprintf_exn;
  ]