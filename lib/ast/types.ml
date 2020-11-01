open Base

open Std

module type Show = sig 
    type t
    val to_string : t -> string
end

(* 
let paren (type a) (module S : Show with type t = a) (v : a) =
    "(" ^ (S.to_string v) ^ ")"
*)

let paren (s : string) : string =
    "(" ^ s ^ ")"

let brack (s : string) : string =
    "{" ^ s ^ "}"

let brace (s : string) : string =
    "[" ^ s ^ "]"

module Type = struct 
    type t =
        | Prim of string
        | Fn of t list * t
    [@@deriving show]

end

module Field = struct 
    type t = 
        { name : string
        ; ty   : Type.t
        }
    [@@deriving show]

end

module Exp = struct
    type prim = 
        | PInt of int
        | PString of string

    and t =
        | App of t * t
        | Ref of string
        | Prim of prim
        | Access of t * string
        | Call of t
    [@@deriving show]

end

module Stm = struct
    type t =
        | Block of t list
        | Let of string * Type.t * Exp.t
        | Label of string
        | Exp of Exp.t
        | Return of Exp.t option
    [@@deriving show]

end

module Fn = struct 
    type t =
        { name : string
        ; args : Field.t list
        ; ty   : Type.t
        ; stm : Stm.t
        }
    [@@deriving show]
end

module Struct = struct 
    type t =
        { name : string 
        ; fs   : Field.t list
        }
    [@@deriving show]

end

module Toplevel = struct
    type t = 
        | Struct of Struct.t 
        | Fn of Fn.t
    [@@deriving show]

end

module Module = struct

    type t =
        { file : File.t
        ; name : string
        ; tls  : Toplevel.t list
        ; incs : string list
        }
    [@@deriving show]
end
