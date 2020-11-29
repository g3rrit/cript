open Std

type type_t =
    | Type_prim of string
    | Type_fn of type_t list * type_t

type field_t = 
    { name : string
    ; ty   : type_t
    }

type exp_prim_t = 
    | Exp_prim_int of int
    | Exp_prim_string of string

and exp_t =
    | Exp_app of exp_t * exp_t
    | Exp_ref of string
    | Exp_prim of exp_prim_t
    | Exp_access of exp_t * string
    | Exp_call of exp_t

type stm_var_t = 
    { name : string
    ; ty   : type_t
    ; v    : exp_t
    }

type stm_t =
    | Stm_scope of string option * stm_var_t list * exp_t option * stm_t list
    | Stm_let of stm_var_t
    | Stm_exp of exp_t
    | Stm_return of exp_t option
    | Stm_jump of string option
    | Stm_break of string option
    | Stm_defer of stm_t list
    | Stm_fn of fn_t
    | Stm_struct of struct_t

and fn_t =
    { fname : string
    ; args  : field_t list
    ; ty    : type_t
    ; stms  : stm_t list
    }

and struct_t =
    { sname : string 
    ; fs    : field_t list
    }

type toplevel_t = 
    | Toplevel_struct of struct_t
    | Toplevel_fn of fn_t

type module_t =
    { file : File.t
    ; name : string
    ; tls  : toplevel_t list
    ; incs : string list
    }
