open Namespace
open Core

let ident = ref 1

let next_ident () = 
    if !ident >= 0x1000 
    then assert false
    else let v = !ident in Int.incr ident; v

let mod_id = 0

type prim_type = 
    { name : string
    ; id   : int
    }

let ty_int = { name = "int"; id = 0x1 }

let prim_type_list : prim_type list =
    [ ty_int
    ]

type prim_function =
    { name : string
    ; args : prim_type list
    ; ret  : prim_type
    ; id   : int
    }

let fn_add = { name = "add"; args = [ty_int; ty_int]; ret = ty_int; id = 0x101 }

let prim_function_list : prim_function list =
    [ fn_add
    ]


let prim_ty_ns : (int * string_type_map) = 
    (mod_id, Map.of_alist_exn (module String)
        (List.map prim_type_list ~f:(fun t -> (t.name, { id = t.id; fs = (Map.empty (module String))}))))

let prim_fn_ns : (int * string_fn_map) =
    (mod_id, Map.of_alist_exn (module String)
        (List.map prim_function_list ~f:(fun f ->
            (f.name, ({ id = f.id; ty =
                ((List.map f.args ~f:(fun a -> I.Type.Prim a.id)),
                    I.Type.Prim f.ret.id ) } : fn_ns) )
        )))