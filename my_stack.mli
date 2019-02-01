(**************************************************************************)
(*                                                                        *)
(*                  Adrien Blassiau, projet IPF 2018-2019                 *)
(*                                                                        *)
(*      Interface de fonctions liées à la construction d'une pile (FILO). *)
(*                                                                        *)
(**************************************************************************)

exception TooShort

type 'a stack =
  | Vide
  | Maillon of int * 'a * 'a stack

val new_stack :
  'a stack

val get_size :
  'a stack ->
  int

val push :
  'a ->
  'a stack ->
  'a stack

val pop :
  'a stack ->
  ('a option * 'a stack )

val get_first :
  'a stack  ->
  'a

val get_first_and_second :
  'a stack  ->
  'a * 'a

val fold :
  ('a -> 'b -> 'b) ->
  'a stack ->
  'b ->
  'b

val print_stack :
  ('a -> unit) ->
  'a stack  ->
  unit

