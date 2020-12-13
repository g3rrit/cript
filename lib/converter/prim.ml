open Namespace
open Core

module A = Ast.Types
module I = Ir.Types

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

let ty_void = { name = "void"; id = 0x1 }
let ty_int = { name = "int"; id = 0x2 }
let ty_bool = { name = "bool"; id = 0x3 }

let to_ir_ty (pt : prim_type) : I.Type.t =
    I.Type.Prim pt.id

let ir_ty_void = to_ir_ty ty_void
let ir_ty_int = to_ir_ty ty_int
let ir_ty_bool = to_ir_ty ty_bool

let prim_type_list : prim_type list =
    [ ty_void
    ; ty_int
    ; ty_bool
    ]

type prim_function =
    { name : string
    ; args : prim_type list
    ; ret  : prim_type
    ; id   : int
    }

let fn_add = { name = "add"; args = [ty_int; ty_int]; ret = ty_int; id = 0x101}
let fn_print_int = { name = "print_int"; args = [ty_int]; ret = ty_void; id = 0x102}

let prim_function_list : prim_function list =
    [ fn_add
    ; fn_print_int
    ]

let fn_to_ir_ty (pf : prim_function) : I.Type.t =
    I.Type.Fn (List.map pf.args ~f:to_ir_ty, to_ir_ty pf.ret)

let fn_to_ir_exp (pf : prim_function) : I.Exp.t =
    I.Exp.Ref (pf.id, fn_to_ir_ty pf)

let ir_fn_add_exp = fn_to_ir_exp fn_add

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
