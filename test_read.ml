(**************************************************************************)
(*                                                                        *)
(*              Julien Forest, Adrien Blassiau, projet IPF 2018-2019      *)
(*                                                                        *)
(*      Test des fonctions de lecture de fichiers (surtout les fonctions  *)
(*      annexes ...)                                                      *)
(*                                                                        *)
(**************************************************************************)

open Test
open Read
open Scanf
open Printf

module Mtriplet = struct
  type t = (string * float * float)
  let construct (s,f1,f2) = s,f1,f2
end

module Tread= MakeReader(Mtriplet)
open Tread

let tests_read_bool =
  [ "split_on_string_empty_perso",
    (fun () ->
       split_on_string_perso "" 0 "" [] = [""]),
    true, fprintf_bool;
    "split_on_string_not_empty_perso",
    (fun () ->
       split_on_string_perso "Hello world" 0 "" [] = ["Hello";"world"]),
    true, fprintf_bool;]


let tests_read_exn  =
  [ "file_not_found",
    (fun () ->
       try
         ignore @@ get_infos_from_file false "toto.txt" ~verbose:false;
         raise @@ Error ("Should raise",File_not_found("toto.txt"))
       with e -> e),
    File_not_found("toto.txt"), fprintf_exn;
  ]
