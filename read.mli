(**************************************************************************)
(*                                                                        *)
(*              Julien Forest, Adrien Blassiau, projet IPF 2018-2019      *)
(*                                                                        *)
(*      Interfaces d'un foncteur de lecture de fichiers, une              *)
(*      partie étant déjà fournie                                         *)
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

module MakeReader (N : ConstructedTypeRead) : (R with type elt = N.t)