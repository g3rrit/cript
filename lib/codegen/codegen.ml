open Std
open Core
open Converter.Prim

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
        | Prim p -> if List.exists prim_type_list ~f:(fun a -> Int.equal a.id p)
                    then List.iter prim_type_list ~f:(fun a -> write fd "%s " a.name)
                    else write fd "i%x " p
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

let cgen_field (fd : Out_channel.t) (arg : Field.t) : unit = 
    cgen_type_start fd arg.ty
    ; write fd "i%x" arg.id
    ; cgen_type_end fd arg.ty

let cgen_fn_decl (fd : Out_channel.t) (f : Fn.t) : unit =
    cgen_type_start fd f.ty 
    ; write fd "i%x (" f.id 
    ; write_list fd f.args ~sep:"," ~f:cgen_field
    ; write fd ")"
    ; cgen_type_end fd f.ty

let cgen_struct_fd (fd : Out_channel.t) (s : Struct.t) : unit =
    write fd "struct i%x" s.id

(* DEFINITIONS *)

let cgen_struct (fd: Out_channel.t) (s : Struct.t) : unit =
    write fd "struct i%x {\n" s.id
    ; List.iter s.fs ~f:(fun m -> cgen_field fd m; write fd ";\n")
    ; write fd "}"

let rec cgen_exp (fd: Out_channel.t) (e: Exp.t) : unit =
    let cgen_exp_prim = function
        | Exp.PInt v    -> write fd "%d" v
        | Exp.PString v -> write fd "\"%s\"" v
    in
    match e with 
        | Exp.App (e, args, _) -> write fd "("
                                  ; cgen_exp fd e
                                  ; write fd ")("
                                  ; write_list fd args ~sep:"," ~f:cgen_exp
                                  ; write fd ")"
        | Exp.Ref (n, _) -> write fd "i%x" n
        | Exp.Prim p     -> cgen_exp_prim p
        | Exp.Access (e, m, _) -> write fd "(("
                            ; cgen_exp fd e
                            ; write fd ").i%x" m
                            ; write fd ")"

let cgen_var (fd: Out_channel.t) (v : Stm.var) : unit = 
     cgen_type_start fd v.ty
    ; write fd "i%x" v.id
    ; cgen_type_end fd v.ty
    ; write fd " = "
    ; cgen_exp fd v.v
    ; write fd ";\n"

let rec cgen_stm (fd: Out_channel.t) (stm: Stm.t) : unit =
    match stm with
        | Stm.Scope (n, vs, me, ss) -> write fd "{\n"
                                       ; List.iter vs ~f:(cgen_var fd)
                                       ; write fd "l%x_start:\n" n
                                       ; Option.iter me ~f:(fun e -> write fd "if (!("
                                                                     ; cgen_exp fd e
                                                                     ; write fd ")) { goto l%x_end; }\n" n)
                                       ; write fd "{\n"
                                       ; (match ss with 
                                            | [] -> assert false
                                            | s :: _ -> cgen_stm fd s)
                                       (*List.iter ss ~f:(cgen_stm fd) *)
                                       ; write fd "}\n}\nl%x_end:\n" n
        | Stm.Let v -> cgen_var fd v
        | Stm.Exp e         -> cgen_exp fd e; write fd ";\n"
        | Stm.Return me     -> write fd "return "
                               ; Option.map me ~f:(cgen_exp fd) |> ignore; write fd ";\n"
        | Stm.Jump n        -> write fd "goto l%x_start;\n" n
        | Stm.Break n       -> write fd "goto l%x_end;\n" n

let cgen_fn (fd: Out_channel.t) (f: Fn.t) : unit =
    cgen_fn_decl fd f
    ; write fd " {\n"
    ; List.iter f.stms ~f:(cgen_stm fd)
    ; write fd "}"


let cgen_module (fd : Out_channel.t) (m : Module.t) : unit =
    write fd "// MODULE (%s)[%x]\n//---------------\n" (File.show m.file) m.id
    ; write fd "\n// STRUCT FORWARD DECLARATIONS\n\n"
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

    