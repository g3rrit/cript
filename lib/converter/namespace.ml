open Std
open Core

module A = Ast.Types
module I = Ir.Types

(* helper types *)
type type_ns = 
    { id : int
    ; fs : string_int_map
    }

type string_type_map = (string, type_ns, String.comparator_witness) Map.t
let string_type_map_show (stm : string_type_map) : string =
    Map.fold stm ~init:"" ~f:(fun ~key:k ~data:d a -> String.concat [a ; k; ": "; Int.to_string d.id; string_int_map_show d.fs; "\n"] ) 

type int_struct_map = (int, I.Struct.t, Int.comparator_witness) Map.t
let int_struct_map_show (ism : int_struct_map) : string =
    Map.fold ism ~init:"" ~f:(fun ~key:_ ~data:d a -> String.concat [a ; I.Struct.show d; "\n"])

type fn_ns = 
    { id : int
    ; ty : (I.Type.t list * I.Type.t)
    }

type string_fn_map = (string, fn_ns, String.comparator_witness) Map.t
let string_fn_map_show (sfm : string_fn_map) : string =
    Map.fold sfm ~init:"" ~f:(fun ~key:k ~data:d a -> String.concat [a ; k; ": "; Int.to_string d.id; "..." ])

type int_fn_map = (int, I.Fn.t, Int.comparator_witness) Map.t
let int_fn_map_show (ifm : int_fn_map) : string =
    Map.fold ifm ~init:"" ~f:(fun ~key:_ ~data:d a -> String.concat [a ; I.Fn.show d; "\n"])

type var_ns =
    { id : int
    ; ty : I.Type.t
    }

type string_var_list = var_ns list

type string_var_map = (string, var_ns, String.comparator_witness) Map.t
let string_var_map_show (svm : string_var_map) : string =
    Map.fold svm ~init:"" ~f:(fun ~key:k ~data:d a -> String.concat [a ; k; ": "; Int.to_string d.id; "#"; I.Type.show d.ty ])

