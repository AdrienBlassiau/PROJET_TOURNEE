(**************************************************************************)
(*                                                                        *)
(*                  Adrien Blassiau, projet IPF 2018-2019                 *)
(*                                                                        *)
(*      Corps et Interfaces d'un foncteur d'affichage graphique           *)
(*      de la tournée utilisant la bibliothèque graphique Graphics        *)
(*      d'OCaml. La plupart des fonctions sont difficilement testables    *)
(*      ici ...                                                           *)
(*                                                                        *)
(**************************************************************************)

module type OrderedTypeSet = sig
  type t
  val compare : t -> t -> int
  val get_infos : t -> (string * float * float)
end

module type P = sig
  type node
  val draw_graph : (int * int) list -> node list -> unit
  val draw_graph_with_modif : (int * int) list -> node list -> node list -> unit
  val draw_graph_with_plan : node list -> (int * int * int * int) list -> unit
end

module MakeDrawer (N : OrderedTypeSet) : (P with type node = N.t) = struct
  open Graphics

  type node = N.t

  let pi = 3.1415927

  (* [make_string_info name x y] fonction d'affichage

     @ensures   le résultat est un triplet tel que 0<=r,g,b<=255
  *)
  let make_string_info name x y =
    name

  (* [draw_node node color] fonction d'affichage d'un point

     @requires  node quelconque et color doit respecter les contraintes d'une
     couleur rgb bien formée
     @ensures   le résultat est affiché sur la fenêtre graphique
  *)
  let draw_node node color =
    let name,x,y = N.get_infos node in
    let int_x = int_of_float x in
    let int_y = int_of_float y in
    let sc = 2 in
    let r,g,b = if color then 255,0,0 else 0,0,0 in
    Graphics.set_color (Graphics.rgb r g b);
    Graphics.fill_circle int_x int_y sc;
    Graphics.set_color (Graphics.rgb r g b);
    Graphics.draw_circle int_x int_y sc;
    Graphics.moveto (int_x+1) (int_y+1);
    Graphics.set_text_size 10;
    Graphics.draw_string (make_string_info name x y)

  (* [draw_edge node_array] fonction d'affichage d'une route

     @requires  node_array une array de points à afficher quelconque
     @ensures   le résultat est affiché sur la fenêtre graphique
  *)
  let draw_edge node_array =
    Graphics.set_line_width 1 ;
    Graphics.set_color (Graphics.rgb 0 0 0);
    Graphics.draw_poly node_array

  (* [draw_plan plan_array] fonction d'affichage des points et des plans de
     découpe de l'arbre kd

     @requires  plan_array une array de points à afficher quelconque
     @ensures   le résultat est affiché sur la fenêtre graphique
  *)
  let draw_plan plan_array =
    Graphics.set_line_width 1 ;
    Graphics.set_color (Graphics.rgb 255 0 0);
    Graphics.draw_segments plan_array;
    Unix.sleep 1


  (* [draw_graph road_list city_list] fonction d'affichage des points (city_list)
     reliés ou non (road_list)

     @requires  road_list et city_list quelconques
     @ensures   le résultat est affiché sur la fenêtre graphique
  *)
  let draw_graph road_list city_list =
    Graphics.open_graph " Projet Tournee";
    Graphics.set_window_title " Enveloppe convexe";
    Random.self_init ();
    Graphics.resize_window 1000 1000;
    Graphics.clear_graph ();
    let node_array = Array.of_list road_list in
    List.fold_left (fun acc node -> draw_node node false) () city_list ;
    draw_edge node_array ;
    Unix.sleep 1

  (* [draw_graph_with_plan city_list plan_list] fonction d'affichage des points
     (city_list) avec les plans les séparants en différents blocs
     @requires  plan_list et city_list quelconques
     @ensures   le résultat est affiché sur la fenêtre graphique
  *)
  let draw_graph_with_plan city_list plan_list =
    Graphics.open_graph " Projet Tournee";
    Graphics.set_window_title " Enveloppe convexe";
    Random.self_init ();
    Graphics.resize_window 1000 1000;
    Graphics.clear_graph ();
    let plan_array = Array.of_list plan_list in
    List.fold_left (fun acc node -> draw_node node false) () city_list ;
    draw_plan plan_array ;
    Unix.sleep 1

  (* [draw_graph_with_modif road_list city_list changed_nodes]
     fonction d'affichage des points (city_list) reliés ou
     non (road_list) avec une coloration des points appartenant à un itinéraire
     modifié

     @requires  road_list, city_list et changed_nodes quelconques
     @ensures   le résultat est affiché sur la fenêtre graphique
  *)
  let draw_graph_with_modif road_list city_list changed_nodes =
    Graphics.open_graph " Projet Tournee";
    Graphics.set_window_title " Enveloppe convexe";
    Random.self_init ();
    Graphics.resize_window 1000 1000;
    Graphics.clear_graph ();
    let node_array = Array.of_list road_list in
    List.fold_left (fun acc node ->draw_node node false) () city_list ;
    List.fold_left (fun acc node ->draw_node node true) () changed_nodes ;
    draw_edge node_array;
    Unix.sleep 1

end