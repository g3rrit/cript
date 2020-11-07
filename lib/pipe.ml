open Std
open Core

module A = Ast
module I = Ir
module C = Converter

module Config = Config
module Std = Std

let parse_mods (fs : File.t list) : A.Types.Module.t list =
    List.map fs ~f:(fun fi -> A.parse fi)

let convert_mods (ms : A.Types.Module.t list) : I.Types.Unit.t = 
    C.init () ; C.convert ms

let rec run (fs : File.t list) : unit = 
    ignore (List.map fs ~f:(fun s -> File.to_string s |> printf "%s\n") : unit list);
    printf "%s\n" (Config.to_string ());
    try pipe fs with | e -> Std.Error.handle e;

and pipe (fs : File.t list) : unit =
    let ms : A.Types.Module.t list = parse_mods fs in 
    ignore ((List.map ~f:(fun m -> A.Types.Module.show m |> Stdio.printf "%s\n") ms) : unit list)
    ; let u = convert_mods ms in 
    Codegen.cgen u



