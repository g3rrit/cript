open Std
open Base
open Core

open Ir.Types

let write fd fmt = Printf.fprintf fd fmt

let rec write_list fd xs ~sep:s ~f:a = 
    match xs with
        | [] -> ()
        | x :: [] -> a fd x
        | x :: xs -> a fd x; write fd "%s" s; write_list fd xs ~sep:s ~f:a

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

let cgen_arg (fd : Out_channel.t) (arg : Field.t) : unit = 
    cgen_type_start fd arg.ty
    ; write fd "i%d" arg.id
    ; cgen_type_end fd arg.ty

let cgen_fn_decl (fd : Out_channel.t) (f : Fn.t) : unit =
    cgen_type_start fd f.ty 
    ; write fd "i%d (" f.id 
    ; write_list fd f.args ~sep:"," ~f:cgen_arg
    ; write fd ")"
    ; cgen_type_end fd f.ty

let cgen_struct_fd (fd : Out_channel.t) (s : Struct.t) : unit =
    write fd "struct i%d" s.id

let cgen_module (fd : Out_channel.t) (m : Module.t) : unit =
    write fd "// Module (%s)[%d]\n" (File.show m.file) m.id
    ; Map.iter m.ds ~f:(fun s -> cgen_struct_fd fd s; write fd ";\n")
    ; Map.iter m.fs ~f:(fun f -> cgen_fn_decl fd f; write fd ";\n")

let cgen (u : Unit.t) : unit =
    let (file, fd) = Filename.open_temp_file "mlc_c_src" ".mlc" in
    Stdio.printf "Writing c src to [%s]\n" file
    ; Map.iter u.mods ~f:(cgen_module fd) 

    