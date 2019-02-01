(**************************************************************************)
(*                                                                        *)
(*                  Adrien Blassiau, projet IPF 2018-2019                 *)
(*                                                                        *)
(*      Module de fonctions liées à la construction d'une pile (FILO).    *)
(*                                                                        *)
(**************************************************************************)

exception TooShort

type 'a stack =
  | Vide
  | Maillon of int * 'a * 'a stack

let new_stack = Vide


(* [is_empty st] retourne un booléen qui vaut vrai si la pile est vide et
   faux dans le cas échéant.

   @requires  st est une pile quelconque
   @ensures   le résultat est booléen
*)
let is_empty st = st = Vide

(* [get_size stack] retourne la taille de la pile passée en argument.

   @requires  stack est une pile quelconque
   @ensures   le résultat est entier positif
*)
let get_size stack = match stack with
  | Vide -> 0
  | Maillon (size, elt, st) -> size

(* [push elt stack] place un élément elt au sommet de la pile stack.

   @requires  elt et stack quelconques
   @ensures   le résultat est la pile avec un élément de plus et donc une
              taille incrémentée de 1
*)
let push elt stack =
  let size = get_size stack in
  Maillon (size + 1, elt, stack)

(* [push elt stack] enlève l'élément se trouvant au sommet de la pile s'il existe

   @requires  stack une pile quelconque
   @ensures   le résultat est un couple composée de l'élément retiré sous forme
              d'option et la pile avec un élément de moins et donc une taille
              décrémentée de 1
*)
let pop stack = match stack with
  | Vide -> None, Vide
  | Maillon (_,elt,st) -> Some elt, st

(* [get_first stack] retourne l'élément se trouvant au sommet de la pile

   @requires  stack une pile quelconque
   @ensures   le résultat est l'élément se trouvant au sommet de la pile s'il existe
   @raises    TooShort si la pile est vide
*)
let get_first stack = match stack with
  | Vide -> raise TooShort
  | Maillon (_,elt,_) -> elt

(* [get_first_and_second stack] retourne l'élément se trouvant au sommet de la pile
   et le suivant

   @requires  stack une pile quelconque
   @ensures   le résultat sont les éléments se trouvant au sommet de la pile
              ainsi que le suivant, s'ils existent
   @raises    TooShort si la pile est de taille inférieure à 2
*)
let get_first_and_second stack = match stack with
  | Vide -> raise TooShort
  | Maillon (_,elt,Vide) -> raise TooShort
  | Maillon (_,elt1, Maillon (_,elt2,_)) -> elt1, elt2

(* [fold f stack v0] applique une fonction à tous les éléments de la pile avec
   comme accumulateur de départ v0

   @requires  f, stack et v0 quelconques
   @ensures   le résultat sont les éléments de la pile auxquels on a appliqué
              la fonction f avec comme accumulateur de départ v0
*)
let rec fold f stack v0 = match stack with
  | Vide -> v0
  | Maillon (_,elt,suite) -> f elt (fold f suite v0)

(* [print_stack print_fun stack] affiche les éléments de la pile avec la fonction
   print_fun

   @requires  print_fun et stack quelconques
   @ensures   le résultat est affiché sur la sortie standard
*)
let rec print_stack print_fun stack =
  fold (fun elt _ ->
      print_endline "" ;
      print_endline "[" ;
      print_fun elt;
      print_string "]" ;
      print_endline " -> ")
    stack ()