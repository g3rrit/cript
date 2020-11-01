open Std
open Base

module A = Ast.Types
module I = Ir.Types

module Conv = struct

    (* helper types *)
    type type_ns = 
        { id : int
        ; fs : string_int_map
        }

    type string_type_map = (string, type_ns, String.comparator_witness) Map.t
    let string_type_map_show (stm : string_type_map) : string =
        Map.fold stm ~init:"" ~f:(fun ~key:k ~data:d a -> String.concat [a ; k; ": "; Int.to_string d.id; string_int_map_show d.fs; "\n"] ) 

    type int_struct_map = (int, I.Struct.t, Int.comparator_witness) Map.t
    let int_struct_map_show (ism : int_struct_map) : string =
        Map.fold ism ~init:"" ~f:(fun ~key:_ ~data:d a -> String.concat [a ; I.Struct.show d; "\n"])

    type fn_ns = 
        { id : int
        ; ty : (I.Type.t list * I.Type.t)
        }

    type string_fn_map = (string, fn_ns, String.comparator_witness) Map.t
    let string_fn_map_show (sfm : string_fn_map) : string =
        Map.fold sfm ~init:"" ~f:(fun ~key:k ~data:d a -> String.concat [a ; k; ": "; Int.to_string d.id; "..." ])

    type int_fn_map = (int, I.Fn.t, Int.comparator_witness) Map.t
    let int_fn_map_show (ifm : int_fn_map) : string =
        Map.fold ifm ~init:"" ~f:(fun ~key:_ ~data:d a -> String.concat [a ; I.Fn.show d; "\n"])
    
    type var_ns =
        { id : int
        ; ty : I.Type.t
        }

    type string_var_list = var_ns list

    type string_var_map = (string, var_ns, String.comparator_witness) Map.t
    let string_var_map_show (svm : string_var_map) : string =
        Map.fold svm ~init:"" ~f:(fun ~key:k ~data:d a -> String.concat [a ; k; ": "; Int.to_string d.id; "#"; I.Type.show d.ty ])

    (* converter functions *)
    let cident = ref 0

    let next_ident () = let ii = !cident in cident := !cident + 1; ii

    let init () = 
        cident := 0
        
    let fold_struct (f : 'a -> A.Struct.t -> 'a) (a: 'a) (m : A.Module.t) : 'a = 
        List.fold_left m.tls ~init:a ~f:(fun aa tl -> (match tl with
                | A.Toplevel.Struct s -> f aa s
                | A.Toplevel.Fn _     -> aa
            ))

    let fold_fn (f : 'a -> A.Fn.t -> 'a) (a: 'a) (m : A.Module.t) : 'a = 
        List.fold_left m.tls ~init:a ~f:(fun aa tl -> (match tl with
                | A.Toplevel.Struct _ -> aa
                | A.Toplevel.Fn ff    -> f aa ff
            ))

    let get_ty_ns (m : A.Module.t) : string_type_map = 
        fold_struct (fun ns s -> 
            let ci = next_ident () in
            Map.add_exn ns ~key:s.name 
                           ~data:{ id = ci
                                 ; fs = List.fold_left s.fs ~init:(Map.empty (module String)) 
                                                            ~f:(fun a fd -> Map.add_exn a ~key:fd.name ~data:(next_ident ()))
                                 }
        ) (Map.empty (module String)) m

    let rec get_ty (ns : string_type_map) (t : A.Type.t) : I.Type.t =
        match t with
            | A.Type.Prim s     -> I.Type.Prim (let sns = Map.find_exn ns s in sns.id)
            | A.Type.Fn (ts, t) -> I.Type.Fn (List.map ts ~f:(get_ty ns), get_ty ns t)

    let get_struct (m : A.Module.t) (ty_ns : string_type_map) : int_struct_map = 
        fold_struct (fun ss s -> 
            let sns = Map.find_exn ty_ns s.name in
            Map.add_exn ss ~key:sns.id
                           ~data:(({ id = sns.id
                                 ; fs = (List.map s.fs 
                                            ~f:(fun fd -> ({ id = Map.find_exn sns.fs fd.name
                                                          ; ty = (get_ty ty_ns fd.ty )
                                                          } : I.Field.t)))
                                 }) : I.Struct.t)
        ) (Map.empty (module Int)) m

    let get_fn_ns (m : A.Module.t) (ty_ns : string_type_map) : string_fn_map = 
        fold_fn (fun ff f -> 
            let ci = next_ident () in
            Map.add_exn ff ~key:f.name 
                           ~data:({ id = ci
                                 ; ty = ((List.map f.args 
                                    ~f:(fun a -> get_ty ty_ns a.ty)), get_ty ty_ns f.ty)
                                 } : fn_ns)
        ) (Map.empty (module String)) m

    let get_field (f : A.Field.t) (var_ns : string_var_map) : I.Field.t =
        let v = Map.find_exn var_ns f.name in
        { id = v.id
        ; ty = v.ty
        }

    let get_args_ns (args : A.Field.t list) (ty_ns : string_type_map) : string_var_map = 
        List.fold args ~init:(Map.empty (module String)) 
                       ~f:(fun am a -> Map.add_exn am ~key:a.name
                                                      ~data:{ id = next_ident ()
                                                            ; ty = get_ty ty_ns a.ty
                                                            })

    let rec get_exp (e : A.Exp.t) (ty_ns : string_type_map) (fn_ns : string_fn_map) (var_ns : string_var_map) (structs : int_struct_map): I.Exp.t =
        let rec flat_app = begin function 
            | A.Exp.App (l, r) -> r :: (flat_app l) 
            | A.Exp.Call e     -> [e] 
            | _                -> raise (Std.Error.Err "unable to apply arguments to non call expression")
        end in  
        let get_pexp = begin function
            | A.Exp.PInt v    -> I.Exp.PInt v
            | A.Exp.PString v -> I.Exp.PString v 
        end in
        match e with
            | A.Exp.Prim p        -> I.Exp.Prim (get_pexp p)
            | A.Exp.Ref s         -> let v = (Map.find_exn var_ns s) in I.Exp.Ref (v.id, v.ty)
            | A.Exp.Call e        -> I.Exp.Call (get_exp e ty_ns fn_ns var_ns structs)
            | A.Exp.Access (l, r) -> 
                let find_ty_by_int i s =  (* this is probably the most inefficient function i have yet written *)
                    match Map.filter ty_ns ~f:(fun a -> Int.equal a.id i) |> Map.data with
                        | [] -> raise (Std.Error.Comp "type that should be present not found")
                        | x :: _ -> Map.find_exn x.fs s
                in
                let le = get_exp e ty_ns fn_ns var_ns structs in
                let lt = I.Exp.get_type le in
                (match lt with
                    | I.Type.Fn _   -> raise (Std.Error.Err "unable to access member of function expression")
                    | I.Type.Prim t -> 
                        let mem = find_ty_by_int t r in
                        let s = Map.find_exn structs t in
                        let mem_f = List.find_exn s.fs ~f:(fun a -> Int.equal a.id mem) in
                        I.Exp.Access (le, mem, mem_f.ty))
            | A.Exp.App (l, r)    -> 
                match flat_app (A.Exp.App (l, r)) |> List.rev with | [] -> assert false | x :: xs ->
                    let le = get_exp x ty_ns fn_ns var_ns structs in 
                    let res = List.map xs ~f:(fun a -> get_exp a ty_ns fn_ns var_ns structs) in
                    let lt = I.Exp.get_type le in
                    let rts = List.map res ~f:(fun a -> I.Exp.get_type a) in
                    match lt with
                        | I.Type.Prim _   -> raise (Std.Error.Err "unable to apply expression to primary expression")
                        | I.Type.Fn (fl, fr) -> 
                            if Int.equal (List.length rts) (List.length fl) |> not
                            then raise (Std.Error.Err "not enough arguments for function")
                            else if List.fold2_exn fl rts ~init:true ~f:(fun c a b -> c && (I.Type.equal a b)) |> not
                                 then raise (Std.Error.Err "types of arguments do not match function type")
                                 else I.Exp.App (le, res, fr)


    let rec get_stm (s : A.Stm.t) (ty_ns : string_type_map) (fn_ns : string_fn_map) (var_ns : string_var_map) (structs : int_struct_map) : (I.Stm.t * string_var_map) =
        match s with
            | A.Stm.Block ss      -> 
                (I.Stm.Block 
                    (let (r, _) = (List.fold ss ~init:([], var_ns)  (* TODO: maybe fold other way here *)
                                                ~f:(fun acc a -> 
                                                    let (ss, var_ns) = acc in
                                                    let (s, nvar_ns) = (get_stm s ty_ns fn_ns var_ns structs) in
                                                    (s :: ss, nvar_ns)))
                     in r), var_ns)
            | A.Stm.Let (n, t, v) -> 
                let lt = get_ty ty_ns t in
                let ni = next_ident () in
                let nvar_ns = Map.update var_ns n ~f:(fun _ -> {id = ni; ty = lt }) in
                (I.Stm.Let (ni, lt, get_exp v ty_ns fn_ns var_ns structs), nvar_ns)
            | A.Stm.Label _       -> (I.Stm.Label (next_ident ()), var_ns) (* TODO: label_ns *)
            | A.Stm.Exp e         -> (I.Stm.Exp (get_exp e ty_ns fn_ns var_ns structs), var_ns)
            | A.Stm.Return me     -> (I.Stm.Return (Option.map me ~f:(fun a -> get_exp a ty_ns fn_ns var_ns structs)), var_ns)


    let get_fn (m : A.Module.t) (ty_ns : string_type_map) (fn_ns : string_fn_map) (structs : int_struct_map): int_fn_map =
        fold_fn (fun ff f -> 
            let fns = Map.find_exn fn_ns f.name in
            let var_ns = get_args_ns f.args ty_ns in
            Map.add_exn ff ~key:fns.id
                           ~data:({ id = fns.id
                                 ; args = List.map f.args ~f:(fun a -> get_field a var_ns)
                                 ; ty = (let (_, r) = fns.ty in r)
                                 ; stm = let (s, _) = get_stm f.stm ty_ns fn_ns var_ns structs in s
                                 } : I.Fn.t)
        ) (Map.empty (module Int)) m

    let convert (ms : A.Module.t list) =
        let ty_ns = Std.Map.merge_list (module String) (List.map ~f:(fun m -> get_ty_ns m) ms) (fun ~key:_ v0 _ -> v0) in
        let structs = List.map ~f:(fun m -> get_struct m ty_ns) ms in
        let fn_ns = Std.Map.merge_list (module String) (List.map ~f:(fun m -> get_fn_ns m ty_ns) ms) (fun ~key:_ v0 _ -> v0) in
        let fns = List.map ~f:(fun m -> get_fn m ty_ns fn_ns structs) ms in
        Stdio.printf "STRUCT:\n%s\nFUNCTION_NS:\n%sFUNCTION:\n%s\n" 
            (List.map structs ~f:int_struct_map_show |> String.concat) 
            (string_fn_map_show fn_ns)
            (List.map fns ~f:int_fn_map_show |> String.concat) 


        


end