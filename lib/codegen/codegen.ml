open Std
open Base
open Core

open Ir.Types

let write fd fmt = Printf.fprintf fd fmt

(* UTILITY FUNCTIONS *)

let rec write_list fd xs ~sep:s ~f:a = 
    match xs with
        | [] -> ()
        | x :: [] -> a fd x
        | x :: xs -> a fd x; write fd "%s" s; write_list fd xs ~sep:s ~f:a

(* DECLARATIONS *)

let rec cgen_type_start (fd : Out_channel.t) (t : Type.t) : unit =
    match t with
        | Prim p -> write fd "i%d " p
        | Fn (_, r) -> cgen_type_start fd r; write fd "(*" 

let rec cgen_type_end (fd : Out_channel.t) (t : Type.t) : unit =
    match t with
        | Prim _ -> ()
        | Fn (l, r) -> write fd ")(" 
                       ; write_list fd l ~sep:"," 
                                         ~f:(fun fd t -> cgen_type_start fd t
                                                        ; cgen_type_end fd t)
                       ; write fd ")"
                       ; cgen_type_end fd r

let cgen_type (fd: Out_channel.t) (t : Type.t) : unit =
    cgen_type_start fd t; cgen_type_end fd t

let cgen_var (fd : Out_channel.t) (arg : Field.t) : unit = 
    cgen_type_start fd arg.ty
    ; write fd "i%d" arg.id
    ; cgen_type_end fd arg.ty

let cgen_fn_decl (fd : Out_channel.t) (f : Fn.t) : unit =
    cgen_type_start fd f.ty 
    ; write fd "i%d (" f.id 
    ; write_list fd f.args ~sep:"," ~f:cgen_var
    ; write fd ")"
    ; cgen_type_end fd f.ty

let cgen_struct_fd (fd : Out_channel.t) (s : Struct.t) : unit =
    write fd "struct i%d" s.id

(* DEFINITIONS *)

let cgen_struct (fd: Out_channel.t) (s : Struct.t) : unit =
    write fd "struct i%d {\n" s.id
    ; List.iter s.fs ~f:(fun m -> cgen_var fd m; write fd ";\n")
    ; write fd "}"

let rec cgen_exp (fd: Out_channel.t) (e: Exp.t) : unit =
    let cgen_exp_prim = function
        | Exp.PInt v    -> write fd "%d" v
        | Exp.PString v -> write fd "%s" v
    in
    match e with 
        | Exp.App (e, args, _) -> write fd "("
                                  ; cgen_exp fd e
                                  ; write fd ")("
                                  ; write_list fd args ~sep:"," ~f:cgen_exp
                                  ; write fd ")"
        | Exp.Ref (n, _) -> write fd "i%d" n
        | Exp.Prim p     -> cgen_exp_prim p
        | Exp.Access (e, m, _) -> write fd "(("
                            ; cgen_exp fd e
                            ; write fd ").i%d" m
                            ; write fd ")"

let rec cgen_stm (fd: Out_channel.t) (stm: Stm.t) : unit =
    match stm with
        | Stm.Block ss      -> List.iter ss ~f:(cgen_stm fd)
        | Stm.Let (n, t, e) -> cgen_var fd { id = n; ty = t }
                               ; cgen_exp fd e
                               ; write fd ";\n"
        | Stm.Label l       -> write fd "i%d:\n" l
        | Stm.Exp e         -> cgen_exp fd e; write fd ";\n"
        | Stm.Return me     -> write fd "return "
                               ; Option.map me ~f:(cgen_exp fd) |> ignore; write fd ";\n"

let cgen_fn (fd: Out_channel.t) (f: Fn.t) : unit =
    cgen_fn_decl fd f
    ; write fd " {\n"
    ; cgen_stm fd f.stm
    ; write fd "}"


let cgen_module (fd : Out_channel.t) (m : Module.t) : unit =
    write fd "// MODULE (%s)[%d]\n//---------------\n" (File.show m.file) m.id
    ; write fd "\n// STRUCT FORWARD DECLARAIONS\n\n"
    ; Map.iter m.ds ~f:(fun s -> cgen_struct_fd fd s; write fd ";\n")
    ; write fd "\n// FUNCTION FORWARD DECLARAIONS\n\n"
    ; Map.iter m.fs ~f:(fun f -> cgen_fn_decl fd f; write fd ";\n")
    ; write fd "\n// STRUCT DECLARAIONS\n\n"
    ; Map.iter m.ds ~f:(fun s -> cgen_struct fd s; write fd ";\n")
    ; write fd "\n// FUNCTION DEFINTIONS\n\n"
    ; Map.iter m.fs ~f:(fun f -> cgen_fn fd f; write fd "\n")
    ; write fd "\n// MODULE_END\n"


let cgen (u : Unit.t) : unit =
    let (file, fd) = Filename.open_temp_file "mlc_c_src" ".c" in
    Stdio.printf "Writing c src to [%s]\n" file
    ; Map.iter u.mods ~f:(cgen_module fd) 

    