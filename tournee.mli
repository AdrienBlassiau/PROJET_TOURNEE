(**************************************************************************)
(*                                                                        *)
(*                  Adrien Blassiau, projet IPF 2018-2019                 *)
(*                                                                        *)
(*      Interfaces d'un foncteur de tournée comportant un ensemble        *)
(*      de fonctions permettant de réaliser les différentes étapes        *)
(*      données dans l'énoncé                                             *)
(*                                                                        *)
(**************************************************************************)

module type T = sig

  module CityGraph : Graph.G
  module Drawer : Draw.P
  module ConvexHull : Convex_hull.C

  type ville
  type tournee
  type city_set
  type kd_tree

  type first_step
  type second_step
  type third_step


  val get_convex_hull : ville list -> ville list

  val print_tournee : ville list -> ville list -> unit

  val print_simple_road : ville list -> unit

  val get_string_and_float_road : ville list -> float * string list

  val get_run : unit -> bool

  val get_input_prameters : unit -> (bool * bool)

  val get_parameters : verbose:bool -> path_from_root_param:string -> unit -> (first_step * second_step * third_step)

  val get_cities : ?complet:bool -> verbose:bool -> path_from_root_city:string -> unit -> (ville list * (string * string list) list)

  val set_kd_tree : ville list -> verbose:bool -> graphic:bool -> kd_tree

  val add_cities_to_tournee : (string * string list) list -> tournee option


  val process_step_1 :
    ?tournee:tournee ->
    ?complet:bool ->
    first_step:first_step ->
    verbose:bool ->
    graphic:bool ->
    ville list ->
    kd_tree ->
    (ville list * city_set * kd_tree * bool)

  val process_step_2 :
    ?tournee:tournee ->
    ?complet:bool ->
    second_step:second_step ->
    verbose:bool ->
    graphic:bool ->
    city_set ->
    ville list  ->
    ville list  ->
    kd_tree ->
    ville list

  val process_step_3 :
    ?tournee:tournee ->
    ?complet:bool ->
    third_step:third_step ->
    verbose:bool ->
    graphic:bool ->
    ville list  ->
    ville list  ->
    bool ->
    ville list

end

module MakeTournee (N : Ville.OrderedTypeVille) : (T with type ville = N.t)