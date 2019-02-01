(**************************************************************************)
(*                                                                        *)
(*                  Adrien Blassiau, projet IPF 2018-2019                 *)
(*                                                                        *)
(*      Corps et Interfaces d'un foncteur de Set de villes, utilisé       *)
(*      pour contenir les villes non encore présentes dans la tournée     *)
(*                                                                        *)
(**************************************************************************)

module type OrderedTypeCitySet = sig
  type t
  val compare : t -> t -> int
  val get_infos : t -> (string * float * float)
end

module type C = sig
  type city
  type city_set

  module CitySet : Set.S

  val empty : city_set
  val is_empty : city_set -> bool
  val get_size : city_set -> int
  val get_city : city_set -> city option
  val make_city_set : city list -> city_set
  val remove_city : city -> city_set -> city_set
  val remove_cities : city list -> city_set -> city_set
  val fold : (city -> 'a -> 'a) -> city_set -> 'a -> 'a
  val print_city_set : city_set -> unit
  val city_set_to_list : city_set -> city list
end

module MakeCitySet (N : OrderedTypeCitySet) : (C with type city = N.t) = struct

  module CitySet = Set.Make(N)

  type city = N.t
  type city_set =  CitySet.t

  let empty = CitySet.empty

  (* [is_empty city_set] retourne un booléen qui vaut vrai si le Set est vide et
     faux dans le cas échéant.

     @requires  city_set est un Set quelconque
     @ensures   le résultat est booléen
  *)
  let is_empty city_set = CitySet.is_empty city_set

  (* [get_size city_set] retourne la taille du Set.

     @requires  city_set est un Set quelconque
     @ensures   le résultat est un entier positif
  *)
  let get_size city_set = CitySet.cardinal city_set

  (* [get_city city_set] retourne une ville de la tournée sous forme d'option.

     @requires  city_set est un Set quelconque
     @ensures   le résultat est une ville du Set sous forme d'option si ce dernier
                n'est pas vide et None sinon
  *)
  let get_city city_set =
    try
      let city = CitySet.choose city_set in
      Some city
    with Not_found -> None

  (* [make_city_set road_list] retourne un set contenant les villes de road_list

     @requires  road_list une liste quelconque
     @ensures   le résultat est un set contenant les villes de road_list
  *)
  let make_city_set road_list =
    List.fold_left (fun acc e -> CitySet.add e acc) empty road_list

  (* [remove_city city city_set] retourne une set sans city

     @requires  city et city_set quelconques
     @ensures   le résultat est un set sans city
  *)
  let remove_city city city_set = CitySet.remove city city_set

  (* [remove_cities city_list city_set] retourne une set sans les villes de
     city_list

     @requires  city_list et city_set quelconques
     @ensures   le résultat est un set sans les villes de city_list
  *)
  let remove_cities city_list city_set =
    List.fold_left (fun acc city -> CitySet.remove city acc) city_set city_list

  (* [fold f city_set acc] applique la fonction f à toutes les villes du Set
     city_set avec comme élément de départ acc

     @requires  city_set et acc quelconques
     @ensures   le résultat est construit à partir de toutes les villes du Set,
                auxquelles on a appliqué f
  *)
  let fold f city_set acc = CitySet.fold f city_set acc

  (* [print_city_set city_set] affiche sur la sortie standard le Set de villes

     @requires  city_set quelconque
     @ensures   le résultat est affiché sur la sortie standard
  *)
  let print_city_set city_set =
    CitySet.fold (fun city acc ->
        let name,_,_ = N.get_infos city in
        print_string name
      ; print_string " -> "
      ) city_set ()
  ; print_string "Nb : "
  ; print_int (get_size city_set) ;print_newline ()

  (* [city_set_to_list city_set] retourne les villes du Set dans une liste

     @requires  city_set quelconque
     @ensures   le résultat est une liste avec les mêmes villes que city_set
  *)
  let city_set_to_list city_set = CitySet.elements city_set

end