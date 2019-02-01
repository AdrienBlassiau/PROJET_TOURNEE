(**************************************************************************)
(*                                                                        *)
(*                  Adrien Blassiau, projet IPF 2018-2019                 *)
(*                                                                        *)
(*      Module de fonctions utiles et inclassables dont certaines         *)
(*      que l'on retrouve dans des versions plus récentes d'OCaml         *)
(*      que celle présente sur les machines de l'école ou dans des        *)
(*      packages additionnels.                                            *)
(*                                                                        *)
(**************************************************************************)

exception EmptyList
exception IsNone

let _ = Random.self_init ()


(* [hd_and_tail l] retourne la tête et la queue d'une liste.

    @requires  l une liste quelconque
    @ensures   le résultat est un tuple
    @raises    EmptyList si la liste fournie en entrée est vide
*)
let hd_and_tail l = match l with
  | [] -> raise EmptyList
  | t::q -> (t,q)

(* [is_some elt_opt] retourne vrai si l'élément elt_opt n'est pas None, faux sinon

    @requires  elt_opt une option quelconque
    @ensures   le résultat est un booléen
*)
let is_some elt_opt = match elt_opt with
  | None -> false
  | Some _ -> true

(* [is_none elt_opt] retourne vrai si l'élément elt_opt est None, faux sinon

    @requires  elt_opt une option quelconque
    @ensures   le résultat est un booléen
*)
let is_none elt_opt = match elt_opt with
  | None -> true
  | Some _ -> false

(* [is_none elt_opt] retourne l'élément associé à l'option passé en paramètre

    @requires  elt_opt une option quelconque
    @ensures   le résultat est l'élément en question
    @raises    IsNone si l'élément passé en entrée est None
*)
let get_some elt_opt = match elt_opt with
  | None -> raise IsNone
  | Some v -> v

(* [norme_carre (x1,y1) (x2,y2)] retourne la norme au carré de d'un vecteur
   donnée par les coordonnées de deux points

    @requires  x1,y1,x2,y2 quelconques
    @ensures   le résultat est positif
*)
let norme_carre (x1,y1) (x2,y2) =
  (x2 -. x1)*.(x2 -. x1) +. (y2 -. y1)*.(y2 -. y1)

(* [norme_carre (x1,y1) (x2,y2)] retourne la norme au carré de d'un vecteur
   donnée par les coordonnées de deux points

    @requires  x1,y1,x2,y2 quelconques
    @ensures   le résultat est positif
*)
let norme (x1,y1) (x2,y2) =
  sqrt (norme_carre (x1,y1) (x2,y2))

(* [split l] retourne le "pivot" d'une liste d'élément, c'est-à-dire l'élément
   qui sépare la liste en un nombre d'éléments égaux, ainsi que les deux sous
   listes à droite et à gauche de ce pivot avec tous les éléments à droite
   inférieurs au pivot et ceux à gauche supérieurs au pivot.

    @requires  l est une liste dont les éléments sont triés par ordre croissant
    @ensures   le résultat est un triplet
*)
let split l =
  let len = (List.length l) / 2 in
  let rec split_int l1 acc i = match l1 with
    | [] -> [],None,[]
    | t::q -> if i < len then split_int q (acc@[t]) (i+1) else (acc,Some t,q)
  in
  split_int l [] 0

(* [get_accessible_cities n i] crée une chaîne de caractères utilisées ensuite
   dans la construction du fichier avec les routes pour la partie 2 du projet

    @requires  n et i quelconque
    @ensures   le résultat est une chaîne de caractère
*)
let rec get_accessible_cities n i =
  if i > 0
  then (get_accessible_cities n (i-1))^(if i != n then " "^(string_of_int i) else "")
  else ""

(* [print_city oc i] crée une chaîne de caractères correspondant au nom
   de la ville, suivies de ses coordonnées, tirées aléatoirement.

    @requires  i positif
    @ensures   le résultat est écrit dans oc
*)
let rec print_city oc i = match i with
  | 0 -> ()
  | n->
    print_city oc (n-1)
  ; Printf.fprintf oc "%s %f %f\n"
      (string_of_int n)
      (Random.float 1000.)
      (Random.float 1000.)

(* [print_road oc i tot] crée une chaîne de caractères correspondant au nom
   de la ville, suivies des villes adjacentes

    @requires  i positif
    @ensures   le résultat est écrit dans oc
*)
let rec print_road oc i tot = match i with
  | 0 -> ()
  | n-> let accessible_cities = get_accessible_cities n tot in
    print_road oc (n-1) tot
  ; Printf.fprintf oc "%s :%s\n" (string_of_int n) accessible_cities

(* [shuffle l] mélange la liste l

    @requires  l une liste
    @ensures   une liste de même taille avec les mêmes éléments
*)
let shuffle l =
  let nd = List.map (fun c -> (Random.bits (), c)) l in
  let sond = List.sort compare nd in
  List.map snd sond

(* let () =
  let nb = 10 in
  let oc = open_out "ville.txt" in
  Printf.fprintf oc "%d\n" nb;
  print_city oc nb;
  print_road oc nb nb;
  close_out oc *)