(**************************************************************************)
(*                                                                        *)
(*              Adrien Blassiau, projet IPF 2018-2019                     *)
(*                                                                        *)
(*      Corps des fonctions de tests utilisées dans le projet. Ce sont    *)
(*      pour la grande majorité celles données en TD ...                  *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)


open Printf

exception Error of string * exn

let _ = Random.self_init ()

let test_with f a b =
  let t0 = Sys.time () in
  let res = f a b in
  let t1 = Sys.time () in
  (t1-.t0,res)

let do_test (test_name, test_function, wanted_anwser, print_anwser) =
  try
    let res = test_function () in
    if res = wanted_anwser
    then
      Format.printf "%s : \027[32mOk\027[0m@." test_name
    else
      Format.printf
        "%s : Error : wanted:=%a obtained:=%a@."
        test_name print_anwser wanted_anwser print_anwser res
  with e ->
    Format.printf "%s : Uncaught exception %s@." test_name (Printexc.to_string e)

let rec generate_random_points n =
  if n < 0
  then []
  else (string_of_int n, Random.float 1000.,Random.float 1000. )::(generate_random_points (n-1))

let fprintf_bool fmt b = Format.fprintf fmt "%b" b

let fprintf_int fmt n = Format.fprintf fmt "%d" n

let fprintf_exn fmt e = Format.fprintf fmt "%s" (Printexc.to_string e)

let fprintf_pts fmt (n,x1,x2) = Format.fprintf fmt "(%f/%f)" x1 x2
