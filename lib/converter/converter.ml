open Std
open Core
open Prim

open Namespace

module A = Ast.Types
module I = Ir.Types

module Prim = Prim

(* converter functions *)
let cident = ref 0


let init () = 
    cident := 0x1000 

(* the state is global unlike the namespaces which are scope dependent *)
module State = struct

    let  struct_map : ((int, int_struct_map, Int.comparator_witness) Map.t) ref = ref (Map.empty (module Int))
    let fn_map : ((int, int_fn_map, Int.comparator_witness) Map.t) ref = ref (Map.empty (module Int))

    let cid = ref 0x1000 (* all prior symbols are reserved *)

    let clear () : unit = 
        struct_map := Map.empty (module Int)
        ; fn_map := Map.empty (module Int)

    let get_tys (mid : int) : int_struct_map =
        Map.find_exn !struct_map mid

    let get_fns (mid : int) : int_fn_map =
        Map.find_exn !fn_map mid

    let add_ty (mid : int) (ty : I.Struct.t) : unit =
        struct_map := Map.update !struct_map mid ~f:(fun ms -> 
            match ms with
                | None   -> Map.singleton (module Int) ty.id ty
                | Some s -> Map.add_exn s ~key:ty.id ~data:ty
            )

    let get_ty (mid : int) (sid : int) : I.Struct.t =
        Map.find_exn (get_tys mid) sid

    let add_fn (mid : int) (fn : I.Fn.t) : unit = 
        fn_map := Map.update !fn_map mid ~f:(fun ms -> 
            match ms with
                | None   -> Map.singleton (module Int) fn.id fn
                | Some s -> Map.add_exn s ~key:fn.id ~data:fn
            )

    let get_fn (mid : int) (fid : int) : I.Fn.t =
        Map.find_exn (get_fns mid) fid

    let search_ty (sid : int) : I.Struct.t = (* this function should probably take an error argument *)
        Map.fold !struct_map ~init:None ~f:(fun ~key:_ ~data:d a -> 
            match a with
                | None -> Map.find d sid
                | Some a -> Some a) |> Option.value_exn

    let search_fn (fid : int) : I.Fn.t = (* this function should probably take an error argument *)
        Map.fold !fn_map ~init:None ~f:(fun ~key:_ ~data:d a -> 
            match a with
                | None -> Map.find d fid
                | Some a -> Some a) |> Option.value_exn

    let next_id () : int = let ii = !cid in cid := !cid + 1; ii

end

type cb_t = 
    { return_handle : (I.Exp.t option -> int -> I.Stm.t list)
    }


let fold_structs (m : A.module_t) (f : 'a -> A.struct_t -> 'a) (a: 'a) : 'a = 
    List.fold m.tls ~init:a ~f:(fun aa tl -> (match tl with
            | A.Toplevel_struct s -> f aa s
            | A.Toplevel_fn _     -> aa
        ))

let fold_fns (m : A.module_t) (f : 'a -> A.fn_t -> 'a) (a: 'a) : 'a = 
    List.fold m.tls ~init:a ~f:(fun aa tl -> (match tl with
            | A.Toplevel_struct _ -> aa
            | A.Toplevel_fn ff    -> f aa ff
        ))

let iter_structs (m : A.module_t) (f : A.struct_t -> unit) : unit = 
    List.iter m.tls ~f:(function
            | A.Toplevel_struct s -> f s
            | A.Toplevel_fn _     -> ()
        )

let iter_fns (m : A.module_t) (f : A.fn_t -> unit) : unit = 
    List.iter m.tls ~f:(function
            | A.Toplevel_struct _ -> ()
            | A.Toplevel_fn ff    -> f ff
        )


let add_ty_ns (s : A.struct_t) (ns : string_type_map) : string_type_map =
    let ci = State.next_id () in
    Map.add_exn ns ~key:s.sname  (* depending on wether or not we are inside function we might wanna shadow name here *)
                   ~data:{ id = ci
                         ; fs = List.fold_left s.fs ~init:(Map.empty (module String)) 
                                                    ~f:(fun a fd -> Map.add_exn a ~key:fd.name ~data:(State.next_id ()))
                         }

let get_ty_ns (m : A.module_t) : string_type_map = 
    fold_structs m (fun ns s -> add_ty_ns s ns) (Map.empty (module String))

let rec get_ty (ns : string_type_map) (t : A.type_t) : I.Type.t =
    match t with
        | A.Type_prim s     -> I.Type.Prim (let sns = Map.find_exn ns s in sns.id)
        | A.Type_fn (ts, t) -> I.Type.Fn (List.map ts ~f:(get_ty ns), get_ty ns t)

(* the next two functions basically do the same but the first is for structs inside functions *)
let convert_struct (mid : int) (s : A.struct_t) (ty_ns : string_type_map) : string_type_map =
    let ty_ns = add_ty_ns s ty_ns in
    let sns = Map.find_exn ty_ns s.sname in
    State.add_ty mid (({ id = sns.id
                   ; fs = (List.map s.fs 
                                    ~f:(fun fd -> ({ id = Map.find_exn sns.fs fd.name
                                                   ; ty = (get_ty ty_ns fd.ty )
                                                   } : I.Field.t)))
                         }) : I.Struct.t) ; ty_ns

let convert_structs (mid : int) (m : A.module_t) (ty_ns : string_type_map) : unit = 
    iter_structs m (fun s -> 
        let sns = Map.find_exn ty_ns s.sname in
        State.add_ty mid (({ id = sns.id
                       ; fs = (List.map s.fs 
                                        ~f:(fun fd -> ({ id = Map.find_exn sns.fs fd.name
                                                       ; ty = (get_ty ty_ns fd.ty )
                                                       } : I.Field.t)))
                             }) : I.Struct.t))

let add_fn_ns (f : A.fn_t) (fn_ns : string_fn_map) (ty_ns : string_type_map) : string_fn_map =
    let ci = State.next_id () in
    Map.add_exn fn_ns ~key:f.fname 
                   ~data:({ id = ci
                         ; ty = ((List.map f.args 
                            ~f:(fun a -> get_ty ty_ns a.ty)), get_ty ty_ns f.ty)
                         } : fn_ns)

let get_fn_ns (m : A.module_t) (ty_ns : string_type_map) : string_fn_map = 
    fold_fns m (fun ff f -> add_fn_ns f ff ty_ns) (Map.empty (module String))

let get_field (f : A.field_t) (var_ns : string_var_map) : I.Field.t =
    let v = Map.find_exn var_ns f.name in
    { id = v.id
    ; ty = v.ty
    }

let get_args_ns (args : A.field_t list) (ty_ns : string_type_map) : string_var_map = 
    List.fold args ~init:(Map.empty (module String)) 
                   ~f:(fun am a -> Map.add_exn am ~key:a.name
                                                  ~data:{ id = State.next_id ()
                                                        ; ty = get_ty ty_ns a.ty
                                                        })

let rec get_exp (mid : int) (e : A.exp_t) (ty_ns : string_type_map) (fn_ns : string_fn_map) (var_ns : string_var_map)
    : I.Exp.t =
    let rec flat_app = begin function 
        | A.Exp_app (l, r) -> r :: (flat_app l) 
        | A.Exp_call e     -> [e] 
        | _                -> raise (Std.Error.Err "unable to apply arguments to non call expression")
    end in  
    let get_pexp = begin function
        | A.Exp_prim_int v    -> I.Exp.PInt v
        | A.Exp_prim_string v -> I.Exp.PString v 
    end in
    match e with
        | A.Exp_prim p        -> I.Exp.Prim (get_pexp p)
        | A.Exp_ref s         -> let v = Option.map (Map.find var_ns s) ~f:(fun v -> I.Exp.Ref (v.id, v.ty)) in 
                                 if Option.is_some v
                                 then Option.value_exn v
                                 else let f = (Map.find_exn fn_ns s) in 
                                      let (fty, ftyr) = f.ty
                                      in I.Exp.Ref (f.id, I.Type.Fn (fty, ftyr))
        | A.Exp_call e        -> 
            let le = get_exp mid e ty_ns fn_ns var_ns in
            let t = I.Exp.get_type le in
            (match t with
                | I.Type.Fn ([], r) -> I.Exp.App (le, [], r) (* TODO: better error for not enough arguments *)
                | _ -> raise (Std.Error.Err "Cannot call variable of non function type"))
        | A.Exp_access (l, r) -> 
            let find_ty_by_int i s =  (* this is probably the most inefficient function i have yet written *)
                match Map.filter ty_ns ~f:(fun a -> Int.equal a.id i) |> Map.data with
                    | [] -> raise (Std.Error.Comp "type that should be present not found")
                    | x :: _ -> Map.find_exn x.fs s
            in
            let le = get_exp mid l ty_ns fn_ns var_ns in
            let lt = I.Exp.get_type le in
            (match lt with
                | I.Type.Fn _   -> raise (Std.Error.Err "unable to access member of function expression")
                | I.Type.Prim t -> 
                    let mem = find_ty_by_int t r in
                    let s = State.search_ty mid in
                    let mem_f = List.find_exn s.fs ~f:(fun a -> Int.equal a.id mem) in
                    I.Exp.Access (le, mem, mem_f.ty))
        | A.Exp_app (l, r)    -> 
            match flat_app (A.Exp_app (l, r)) |> List.rev with | [] -> assert false | x :: xs ->
                let le = get_exp mid x ty_ns fn_ns var_ns in 
                let res = List.map xs ~f:(fun a -> get_exp mid a ty_ns fn_ns var_ns ) in
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


let get_var (mid : int) (v : A.stm_var_t) (ty_ns : string_type_map) (fn_ns : string_fn_map) (var_ns : string_var_map)
        : I.Stm.var =
    let lt = get_ty ty_ns v.ty in
    let ni = State.next_id () in
    { id = ni
    ; ty = lt
    ; v = get_exp mid v.v ty_ns fn_ns var_ns
    }

let rec get_stm 
        (mid : int)
        (s : A.stm_t) 
        (cb : cb_t)
        (ty_ns : string_type_map) 
        (fn_ns : string_fn_map) 
        (var_ns : string_var_map) 
        (label_ns : string_int_map)
        (ret_label : int)
        : ((I.Stm.t list) * (I.Stm.t list) * string_var_map * string_type_map * string_fn_map) =
    match s with
        | A.Stm_scope (mn, vs, me, ss) -> 
            let jmp_i = State.next_id () in
            let brk_i = State.next_id () in
            let label_ns = Map.update label_ns ("__jmp" ^ (Option.value mn ~default:"__label")) ~f:(fun _ -> jmp_i) in
            let label_ns = Map.update label_ns ("__brk" ^ (Option.value mn ~default:"__label")) ~f:(fun _ -> brk_i) in
            let (vss, nvar_ns) = List.fold vs ~init:([], var_ns) 
                                   ~f:(fun acc a ->
                                        let (vs, var_ns) = acc in
                                        let v = get_var mid a ty_ns fn_ns var_ns in
                                        let var_ns = Map.update var_ns a.name ~f:(fun _ -> { id = v.id; ty = v.ty } ) in
                                        (v :: vs, var_ns))
            in
            let ne = Option.map me ~f:(fun a -> get_exp mid a ty_ns fn_ns nvar_ns ) in
            let (nss, nssd) = get_stm_list mid ss cb ty_ns fn_ns nvar_ns label_ns ret_label in
            let nss = List.concat 
                [ (List.map vss ~f:(fun v -> I.Stm.Let v) |> List.rev)
                ; [I.Stm.Label jmp_i ]
                ; (match ne with | None -> [] | Some e -> [I.Stm.Jump (brk_i, Some e)])
                ; nss
                ; [I.Stm.Label brk_i]
                ] in
            (nss , nssd, var_ns, ty_ns, fn_ns)
        | A.Stm_let v -> 
            let lt = get_ty ty_ns v.ty in
            let ni = State.next_id () in
            let nvar_ns = Map.update var_ns v.name ~f:(fun _ -> {id = ni; ty = lt }) in
            ([I.Stm.Let ({ id = ni; ty = lt; v = get_exp mid v.v ty_ns fn_ns var_ns })], [], nvar_ns, ty_ns, fn_ns)
        | A.Stm_exp e         -> ([I.Stm.Exp (get_exp mid e ty_ns fn_ns var_ns )], [], var_ns, ty_ns, fn_ns)
        | A.Stm_return me     -> (cb.return_handle (Option.map me ~f:(fun a -> get_exp mid a ty_ns fn_ns var_ns )) ret_label
                                 , [], var_ns, ty_ns, fn_ns)
        | A.Stm_jump mn       -> 
            let v = Map.find_exn label_ns ("__jmp" ^ (Option.value mn ~default:"__label")) in
            ([I.Stm.Jump (v, None)], [], var_ns, ty_ns, fn_ns)
        | A.Stm_break mn       -> 
            let v = Map.find_exn label_ns ("__brk" ^ (Option.value mn ~default:"__label")) in
            ([I.Stm.Jump (v, None)], [], var_ns, ty_ns, fn_ns)
        | A.Stm_defer ss -> let (ss, ssd) = get_stm_list mid ss cb ty_ns fn_ns var_ns label_ns ret_label in 
                            ([], List.concat [ss; ssd], var_ns, ty_ns, fn_ns)
        | A.Stm_struct s -> ([], [], var_ns, convert_struct mid s ty_ns, fn_ns)
        | A.Stm_fn f     -> ([], [], var_ns, ty_ns, convert_fn mid f ty_ns fn_ns)

and get_stm_list 
        (mid : int)
        (ss: A.stm_t list)
        (cb : cb_t)
        (ty_ns  : string_type_map)
        (fn_ns : string_fn_map)
        (var_ns : string_var_map)
        (label_ns : string_int_map)
        (ret_label : int)
        : I.Stm.t list * I.Stm.t list =
    let (r, dr, _, _, _) = List.fold ss ~init:([], [], var_ns, ty_ns, fn_ns) (* TODO: maybe fold other way *)
                              ~f:(fun acc a ->
                                    let (ss, ssd, var_ns, ty_ns, fn_ns) = acc in
                                    let (s, sd, nvar_ns, nty_ns, nfn_ns) = (get_stm mid a cb ty_ns fn_ns var_ns label_ns ret_label) in
                                    (List.concat [List.rev s; ss], List.concat [List.rev sd; ssd], nvar_ns, nty_ns, nfn_ns))
    in (List.rev r, dr)

and convert_fn (mid : int) (f : A.fn_t) (ty_ns : string_type_map) (fn_ns : string_fn_map) : string_fn_map =
    let fn_ns = add_fn_ns f fn_ns ty_ns in
    let fns = Map.find_exn fn_ns f.fname in
    let var_ns = get_args_ns f.args ty_ns in
    let ret_id = State.next_id () in
    let ret_label = State.next_id () in
    let var_ns = (match f.ty with
        | t -> Map.add_exn var_ns ~key:"__ret_val" ~data: { id = ret_id; ty = get_ty ty_ns t }
        (* do something if void *)
        ) in
    let cb = { return_handle = (fun me rl ->
                    match f.ty with
                        | _ -> [ I.Stm.Assign (ret_id, (Option.value_exn me)); I.Stm.Jump (rl, None) ]
                        (* handle void with error if expression contains no return value *)
                    )
             } in
    let (stms, defers)  = get_stm_list mid f.stms cb ty_ns fn_ns var_ns (Map.empty (module String)) ret_label in
    State.add_fn mid ({ id = fns.id
                         ; args = List.map f.args ~f:(fun a -> get_field a var_ns)
                         ; ty = (let (_, r) = fns.ty in r)
                         ; stms = List.concat [stms; defers]
                         ; ret_val = ret_id
                         ; ret_label = ret_label
                         ; tags = [] (* maybe allow main inside function? *)
                         } : I.Fn.t); fn_ns


let convert_fns (mid : int) (m : A.module_t) (ty_ns : string_type_map) (fn_ns : string_fn_map) : unit =
    iter_fns m (fun f -> 
        let fns = Map.find_exn fn_ns f.fname in
        let var_ns = get_args_ns f.args ty_ns in
        let ret_id = State.next_id () in
        let ret_label = State.next_id () in
        let var_ns = (match f.ty with
            | t -> Map.add_exn var_ns ~key:"__ret_val" ~data: { id = ret_id; ty = get_ty ty_ns t }
            (* do something if void *)
            ) in
        let cb = { return_handle = (fun me rl ->
                        match f.ty with
                            | _ -> [ I.Stm.Assign (ret_id, (Option.value_exn me)); I.Stm.Jump (rl, None) ]
                            (* handle void with error if expression contains no return value *)
                        )
                 } in
        let (stms, defers) = get_stm_list mid f.stms cb ty_ns fn_ns var_ns (Map.empty (module String)) ret_label in
        State.add_fn mid ({ id = fns.id
                             ; args = List.map f.args ~f:(fun a -> get_field a var_ns)
                             ; ty = (let (_, r) = fns.ty in r)
                             ; stms = List.concat [stms; defers]
                             ; ret_val = ret_id
                             ; ret_label = ret_label
                             ; tags = if String.equal f.fname "main" then [I.Fn.Main] else []
                             } : I.Fn.t))

let get_mod_ns (ms : A.module_t list) : string_int_map = 
    List.fold ms ~init:(Map.singleton (module String) "__prim" 0) ~f:(fun acc a -> Map.add_exn acc ~key:a.name ~data:(State.next_id ()))

let includes (xs : (int * ('a, 'v, 'cmp) Map.t) list) (m : A.module_t) (mod_ns : string_int_map) (comp : ('a, 'cmp) Map.comparator) : ('a, 'v, 'cmp) Map.t =
    let mids = List.map m.incs ~f:(fun a -> Map.find_exn mod_ns a) in
    let fs   = List.filter xs ~f:(function (a, _) -> List.exists mids ~f:(fun b -> Int.equal a b)) in
    let fss  = List.map fs ~f:(function (_, a) -> a) in
    Std.Map.merge_list comp fss (fun ~key:_ a _ -> a) 

let convert (ms : A.module_t list) : I.Unit.t = (* this is all so inefficient *)
    let mod_ns = get_mod_ns ms in
    let get_mid k = Map.find_exn mod_ns k in
    let ty_nss = prim_ty_ns :: (List.map ~f:(fun m -> (get_mid m.name, get_ty_ns m)) ms) in
    let () = List.iter ~f:(fun m -> convert_structs (get_mid m.name)  m (includes ty_nss m mod_ns (module String))) ms in
    let fn_nss = prim_fn_ns :: (List.map ~f:(fun m -> (get_mid m.name, get_fn_ns m (includes ty_nss m mod_ns (module String)))) ms) in
    let () = List.iter ~f:(fun m -> (convert_fns (get_mid m.name) m
                                (includes ty_nss m mod_ns (module String)) 
                                (includes fn_nss m mod_ns (module String))))
                                ms
    in
    let mods = (List.fold ms ~init:(Map.empty (module Int)) 
                           ~f:(fun acc m -> 
                            let mid = get_mid m.name in
                            Map.add_exn acc ~key:mid
                                            ~data:({ file = m.file
                                                   ; id = mid
                                                   ; fs = State.get_fns mid
                                                   ; ds = State.get_tys mid
                                                   ; incs = List.map m.incs ~f:get_mid
                                                   } : I.Module.t)))
    in
    let main_mod = List.find (Map.data mods) ~f:(fun m -> 
        Map.exists m.fs ~f:(fun f -> List.exists f.tags 
                                ~f:(fun a -> (match a with | I.Fn.Main -> true)))) |> Option.value_exn
    in
    let main = ref 0 in
    let () = Map.iter main_mod.fs ~f:(fun f -> if List.exists f.tags ~f:(fun a -> (match a with | I.Fn.Main -> true)) then main := f.id else ()) in
    { mods = mods
    ; main = !main
    }