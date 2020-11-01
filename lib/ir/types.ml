open Std
open Base

module Type = struct 
    type t =
        | Prim of int
        | Fn of t list * t
    [@@deriving show]

    let rec equal (t0 : t) (t1 : t) : bool =
        match (t0, t1) with
            | (Prim i0, Prim i1) -> i0 = i1
            | (Fn (l0, r0), Fn (l1, r1)) -> 
                if Int.equal (List.length l0) (List.length l1)
                then List.fold2_exn l0 l1 ~init:true ~f:(fun c a b -> c && (equal a b)) && (equal r0 r1)
                else false
            | (Prim _, Fn _) -> false
            | (Fn _, Prim _) -> false
end

module Field = struct 
    type t = 
        { id : int
        ; ty  : Type.t
        }
    [@@deriving show]
end

module Exp = struct
    type prim = 
        | PInt of int
        | PString of string

    and t =
        | App of t * t list * Type.t
        | Ref of int * Type.t
        | Prim of prim
        | Access of t * int * Type.t
        | Call of t
    [@@deriving show]

    let get_ptype = function
        | PInt _    -> Type.Prim 0
        | PString _ -> Type.Prim 1

    let rec get_type = function
        | App (_, _, t)    -> t
        | Ref (_, t)       -> t
        | Access (_, _, t) -> t
        | Call e           -> get_type e
        | Prim p           -> get_ptype p
end

module Stm = struct
    type t =
        | Block of t list
        | Let of int * Type.t * Exp.t
        | Label of int
        | Exp of Exp.t
        | Return of Exp.t option
    [@@deriving show]
end

module Fn = struct 
    type t =
        { id   : int
        ; args : Field.t list
        ; ty   : Type.t
        ; stm  : Stm.t
        }
    [@@deriving show]
end

module Struct = struct 
    type t =
        { id : int 
        ; fs : Field.t list
        }
    [@@deriving show]
end

module Module = struct
    type t =
        { file : File.t
        ; id   : int
        ; cs   : Fn.t list
        ; ds   : Struct.t list
        }
    [@@deriving show]
end

module Unit = struct
    type t =
        { mods : Module.t list
        }
    [@@deriving show]
end