open Namespace
open Core

let ident = ref 1

let next_ident () = 
    if !ident >= 0x1000 
    then assert false
    else let v = !ident in Int.incr ident; v

let mod_id = 0

(* refactor this with modules an such *)
let prim_types : (string * int) list =
    [("int", next_ident ())
    ]

let prim_functions : (string * int) list =
    [("add", next_ident())]

let prim_ty_ns : (int * string_type_map) = 
    (mod_id, Map.of_alist_exn (module String)
        (List.map prim_types ~f:(fun (n, i) -> (n, { id = i; fs = (Map.empty (module String))})))

let prim_fn_ns : (int * string_fn_map) =
    (mod_id, Map.of_alist_exn (module String)
        [ ("add", ({ id = next_ident (); ty = ([I.Type.Prim 1], I.Type.Prim 1) } : fn_ns) )
        ])



