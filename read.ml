(**************************************************************************)
(*                                                                        *)
(*              Julien Forest, Adrien Blassiau, projet IPF 2018-2019      *)
(*                                                                        *)
(*      Corps et Interfaces d'un foncteur de lecture de fichiers,         *)
(*      une grosse partie étant déjà fournie                              *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

module type ConstructedTypeRead = sig
  type t
  val construct : (string * float * float) -> t
end

module type R = sig

  type elt

  exception File_not_found of string

  exception Syntax_error of string*string

  val split_on_string_perso : string -> int -> string -> string list -> string list
  val get_infos_from_file : bool -> string -> verbose:bool -> elt list * (string * string list) list
  val get_params_from_file : string -> (string * string * string)

end

module MakeReader (N : ConstructedTypeRead) : (R with type elt = N.t) = struct

  open Scanf
  open Printf

  type elt = N.t

  exception File_not_found of string

  exception Syntax_error of string*string

  (* [split_on_string_perso s n buf l] découpe une chaîne de caractères sur les
     espaces et renvoie un liste des mots trouvés

     @requires  s quelconque, n inférieure à len(l), buf quelconque et l quelconque
     @ensures   le résultat est une liste
  *)
  let rec split_on_string_perso s n buf l =
    let lg = String.length s in
    if n = lg then buf::l
    else if s.[lg-1-n] = ' ' then split_on_string_perso s (n+1) "" (if (buf="") then l else buf::l)
    else split_on_string_perso s (n+1) ((String.make 1 (s.[lg-1-n]))^buf) l;;


  (* [parse_villes cin nb_villes ~verbose] récupère les villes d'un flux d'entrée
     selon un formatage particulier avec la possibilité d'afficher des informations
     supplémentaires en mettant verbose à true

     @requires  cin, nb_villes et verbose quelconque
     @ensures   le résultat est une liste
  *)
  let rec parse_villes cin nb_villes ~verbose =
    if nb_villes = 0
    then []
    else
      let v =
        N.construct (Scanf.bscanf cin "%s %f %f "
                       (fun na x y ->
                          (if verbose
                           then printf "%s (%f/%f)\n" na x y
                           else ())
                        ; na,x,y))
      in
      v::parse_villes cin (nb_villes -1) ~verbose

  (* [parse_road cin nb_villes ~verbose] récupère les villes et routes associées
     d'un flux d'entrée selon un formatage particulier avec la possibilité
     d'afficher des informations supplémentaires en mettant verbose à true

     @requires  cin, nb_villes et verbose quelconques
     @ensures   le résultat est une liste
  *)
  let rec parse_road cin nb_villes ~verbose =
    if nb_villes = 0
    then []
    else
      let v,vl =
        Scanf.bscanf cin " %[0-9] : %[0-9 ]"
          (fun v vl ->
             (if verbose
              then printf "%s : %s\n" v vl
              else ())
           ; v, vl) in
      let cl = split_on_string_perso vl 0 "" [] in
      (v, cl)::parse_road cin (nb_villes-1) ~verbose

  (* [parse_road cin nb_villes ~verbose]
     récupère les villes sous forme d'une liste et les villes et routes
     associées sous forme d'une autre liste si complet et à false avec la
     possibilité d'afficher des informations supplémentaires en mettant verbose
     à true

     @requires  complet ,~verbose et cin quelconques
     @ensures   le résultat est deux listes
  *)
  let parse_input_city complet ~verbose cin =
    let nb_villes = Scanf.bscanf cin "%d " (fun x -> x) in
    (if verbose then printf "We get %d cities \n" nb_villes else ())
  ; let city_list = List.rev (parse_villes cin nb_villes ~verbose) in
    let road_list =
      if not complet then parse_road cin nb_villes ~verbose else [] in
    city_list, road_list

  (* [parse_input_param cin]
     récupère les paramètres d'entrée du programme stockés dans un fichier

     @requires  cin quelconque
     @ensures   le résultat est un triplet de chaines de caractères
  *)
  let parse_input_param cin =
    let step_1 = Scanf.bscanf cin "%s " (fun x -> x) in
    let step_2 = Scanf.bscanf cin "%s " (fun x -> x) in
    let step_3 = Scanf.bscanf cin "%s " (fun x -> x) in
    step_1, step_2, step_3

  (* [parse_input_param cin]
     fonction qui gère l'ouverture, la lecture et la fermeture des fichiers
     proprement. La lecture est faite par la fonction parse_input_fun passée
     en paramètre

     @requires  file_name et parse_input_fun quelconques
     @ensures   le résultat dépend de la fonction passée en paramètre
     @raises    File_not_found si le fichier n'est pas touvé et Syntax_error
                si sa syntaxe est mauvaise
  *)
  let parse_input_file file_name parse_input_fun =
    try
      let cin = open_in file_name in
      try
        let output = parse_input_fun (Scanning.from_channel cin) in
        let _ = close_in cin in
        output
      with e -> close_in cin;raise e
    with
    | Sys_error _ ->
      raise (File_not_found file_name)
    | Scanf.Scan_failure msg ->
      raise (Syntax_error(file_name,msg))

  (* [get_infos_from_file complet file_name ~verbose
     fonction qui appelle parse_input_file pour la lecture des villes et routes si
     cette dernière est demandée (lecture des routes uniquement activée pour
     la partie 2 avec complet passé à false)

     @requires  complet, file_name et ~verbose quelconques
     @ensures   le résultat dépend de la fonction passée en paramètre
  *)
  let get_infos_from_file complet file_name ~verbose =
    parse_input_file file_name (parse_input_city complet ~verbose)

  (* [get_infos_from_file complet file_name ~verbose
     fonction qui appelle parse_input_file pour la lecture des paramètres d'entrée
     du programme

     @requires  file_name quelconque
     @ensures   le résultat est un triplet de chaînes de caractères
  *)
  let get_params_from_file file_name =
    parse_input_file file_name parse_input_param

end