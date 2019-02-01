(**************************************************************************)
(*                                                                        *)
(*                  Adrien Blassiau, projet IPF 2018-2019                 *)
(*                                                                        *)
(*      Interfaces d'un foncteur de Set de villes, utilisé pour           *)
(*      contenir les villes non encore présentes dans la tournée          *)
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


module MakeCitySet (N : OrderedTypeCitySet) : (C with type city = N.t)