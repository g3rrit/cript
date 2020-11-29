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
    type t = 
        { mutable tys : int_struct_map
        ; mutable fns : int_fn_map
        }

    type string_map = (string, t, String.comparator_witness) Map.t

    let cid = ref 0x1000 (* all prior symbols are reserved *)

    let empty () : t = 
        { tys = Map.empty (module Int)
        ; fns = Map.empty (module Int)
        }

    let clear (s : t) : unit = 
        s.tys <- Map.empty (module Int)
        ; s.fns <- Map.empty (module Int)

    let add_ty (s : t) (ty : I.Struct.t) : unit =
        s.tys <- Map.add_exn s.tys ~key:ty.id ~data:ty

    let add_fn (s : t) (fn : I.Fn.t) : unit = 
        s.fns <- Map.add_exn s.fns ~key:fn.id ~data:fn

    let next_id () : int = let ii = !cid in cid := !cid + 1; ii

    let get_map (ms : A.module_t list) : string_map =
        List.map ms ~f:(fun m -> (m.name, empty ())) |> Map.of_alist_exn (module String)
end


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
let convert_struct (st : State.t) (s : A.struct_t) (ty_ns : string_type_map) : string_type_map =
    let ty_ns = add_ty_ns s ty_ns in
    let sns = Map.find_exn ty_ns s.sname in
    State.add_ty st (({ id = sns.id
                   ; fs = (List.map s.fs 
                                    ~f:(fun fd -> ({ id = Map.find_exn sns.fs fd.name
                                                   ; ty = (get_ty ty_ns fd.ty )
                                                   } : I.Field.t)))
                         }) : I.Struct.t) ; ty_ns

let convert_structs (st : State.t) (m : A.module_t) (ty_ns : string_type_map) : unit = 
    iter_structs m (fun s -> 
        let sns = Map.find_exn ty_ns s.sname in
        State.add_ty st (({ id = sns.id
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

let rec get_exp (st : State.t) (e : A.exp_t) (ty_ns : string_type_map) (fn_ns : string_fn_map) (var_ns : string_var_map) (structs : int_struct_map)
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
            let le = get_exp st e ty_ns fn_ns var_ns structs in
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
            let le = get_exp st l ty_ns fn_ns var_ns structs in
            let lt = I.Exp.get_type le in
            (match lt with
                | I.Type.Fn _   -> raise (Std.Error.Err "unable to access member of function expression")
                | I.Type.Prim t -> 
                    let mem = find_ty_by_int t r in
                    let s = Option.value (Map.find st.tys t) ~default:(Map.find_exn structs t)  in
                    let mem_f = List.find_exn s.fs ~f:(fun a -> Int.equal a.id mem) in
                    I.Exp.Access (le, mem, mem_f.ty))
        | A.Exp_app (l, r)    -> 
            match flat_app (A.Exp_app (l, r)) |> List.rev with | [] -> assert false | x :: xs ->
                let le = get_exp st x ty_ns fn_ns var_ns structs in 
                let res = List.map xs ~f:(fun a -> get_exp st a ty_ns fn_ns var_ns structs) in
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


let get_var (st : State.t) (v : A.stm_var_t) (ty_ns : string_type_map) (fn_ns : string_fn_map) (var_ns : string_var_map) (structs : int_struct_map)
        : I.Stm.var =
    let lt = get_ty ty_ns v.ty in
    let ni = State.next_id () in
    { id = ni
    ; ty = lt
    ; v = get_exp st v.v ty_ns fn_ns var_ns structs
    }

let rec get_stm 
        (st : State.t)
        (s : A.stm_t) 
        (ty_ns : string_type_map) 
        (fn_ns : string_fn_map) 
        (var_ns : string_var_map) 
        (label_ns : string_int_map)
        (structs : int_struct_map)
        : ((I.Stm.t list) * string_var_map * string_type_map * string_fn_map) =
    match s with
        | A.Stm_scope (mn, vs, me, ss) -> 
            let ni = State.next_id () in
            let nlabel_ns = Map.update label_ns (Option.value mn ~default:"__label") ~f:(fun _ -> ni) in
            let (vss, nvar_ns) = List.fold vs ~init:([], var_ns) 
                                   ~f:(fun acc a ->
                                        let (vs, var_ns) = acc in
                                        let v = get_var st a ty_ns fn_ns var_ns structs in
                                        let nvar_ns = Map.update var_ns a.name ~f:(fun _ -> { id = v.id; ty = v.ty } ) in
                                        (v :: vs, nvar_ns))
            in
            let ne = Option.map me ~f:(fun a -> get_exp st a ty_ns fn_ns nvar_ns structs) in
            let nss = get_stm_list st ss ty_ns fn_ns nvar_ns nlabel_ns structs in
            ([I.Stm.Scope (ni, List.rev vss, ne, nss)], var_ns, ty_ns, fn_ns)
        | A.Stm_let v -> 
            let lt = get_ty ty_ns v.ty in
            let ni = State.next_id () in
            let nvar_ns = Map.update var_ns v.name ~f:(fun _ -> {id = ni; ty = lt }) in
            ([I.Stm.Let ({ id = ni; ty = lt; v = get_exp st v.v ty_ns fn_ns var_ns structs})], nvar_ns, ty_ns, fn_ns)
        | A.Stm_exp e         -> ([I.Stm.Exp (get_exp st e ty_ns fn_ns var_ns structs)], var_ns, ty_ns, fn_ns)
        | A.Stm_return me     -> ([I.Stm.Return (Option.map me ~f:(fun a -> get_exp st a ty_ns fn_ns var_ns structs))], var_ns, ty_ns, fn_ns)
        | A.Stm_jump mn       -> 
            let v = Map.find_exn label_ns (Option.value mn ~default:"__label") in
            ([I.Stm.Jump v], var_ns, ty_ns, fn_ns)
        | A.Stm_break mn       -> 
            let v = Map.find_exn label_ns (Option.value mn ~default:"__label") in
            ([I.Stm.Break v], var_ns, ty_ns, fn_ns)
        | A.Stm_struct s -> ([], var_ns, convert_struct st s ty_ns, fn_ns)
        | A.Stm_fn f     -> ([], var_ns, ty_ns, convert_fn st f ty_ns fn_ns structs)

and get_stm_list 
        (st : State.t)
        (ss: A.stm_t list)
        (ty_ns  : string_type_map)
        (fn_ns : string_fn_map)
        (var_ns : string_var_map)
        (label_ns : string_int_map)
        (structs : int_struct_map)
        : I.Stm.t list =
    let (r, _, _, _) = List.fold ss ~init:([], var_ns, ty_ns, fn_ns) (* TODO: maybe fold other way *)
                              ~f:(fun acc a ->
                                    let (ss, var_ns, ty_ns, fn_ns) = acc in
                                    let (s, nvar_ns, nty_ns, nfn_ns) = (get_stm st a ty_ns fn_ns var_ns label_ns structs) in
                                    (List.concat [s; ss], nvar_ns, nty_ns, nfn_ns))
    in (List.rev r)

and convert_fn (st : State.t) (f : A.fn_t) (ty_ns : string_type_map) (fn_ns : string_fn_map) (structs : int_struct_map): string_fn_map =
    let fn_ns = add_fn_ns f fn_ns ty_ns in
    let fns = Map.find_exn fn_ns f.fname in
    let var_ns = get_args_ns f.args ty_ns in
    let stms = get_stm_list st f.stms ty_ns fn_ns var_ns (Map.empty (module String)) structs in
    State.add_fn st ({ id = fns.id
                         ; args = List.map f.args ~f:(fun a -> get_field a var_ns)
                         ; ty = (let (_, r) = fns.ty in r)
                         ; stms = stms
                         ; tags = [] (* maybe allow main inside function? *)
                         (*let (s, _) = get_stm f.stm ty_ns fn_ns var_ns in s*)
                         } : I.Fn.t); fn_ns


let convert_fns (st : State.t) (m : A.module_t) (ty_ns : string_type_map) (fn_ns : string_fn_map) (structs : int_struct_map): unit =
    iter_fns m (fun f -> 
        let fns = Map.find_exn fn_ns f.fname in
        let var_ns = get_args_ns f.args ty_ns in
        let stms = get_stm_list st f.stms ty_ns fn_ns var_ns (Map.empty (module String)) structs in
        State.add_fn st ({ id = fns.id
                             ; args = List.map f.args ~f:(fun a -> get_field a var_ns)
                             ; ty = (let (_, r) = fns.ty in r)
                             ; stms = stms
                             ; tags = if String.equal f.fname "main" then [I.Fn.Main] else []
                             (*let (s, _) = get_stm f.stm ty_ns fn_ns var_ns structs in s*)
                             } : I.Fn.t))

let get_mod_ns (ms : A.module_t list) : string_int_map = 
    List.fold ms ~init:(Map.singleton (module String) "__prim" 0) ~f:(fun acc a -> Map.add_exn acc ~key:a.name ~data:(State.next_id ()))

let includes (xs : (int * ('a, 'v, 'cmp) Map.t) list) (m : A.module_t) (mod_ns : string_int_map) (comp : ('a, 'cmp) Map.comparator) : ('a, 'v, 'cmp) Map.t =
    let mids = List.map m.incs ~f:(fun a -> Map.find_exn mod_ns a) in
    let fs   = List.filter xs ~f:(function (a, _) -> List.exists mids ~f:(fun b -> Int.equal a b)) in
    let fss  = List.map fs ~f:(function (_, a) -> a) in
    Std.Map.merge_list comp fss (fun ~key:_ a _ -> a) 

let convert (ms : A.module_t list) : I.Unit.t = (* this is all so inefficient *)
    let st_map = State.get_map ms in
    let mod_ns = get_mod_ns ms in
    let get_mid k = Map.find_exn mod_ns k in
    let ty_nss = prim_ty_ns :: (List.map ~f:(fun m -> (get_mid m.name, get_ty_ns m)) ms) in
    let () = List.iter ~f:(fun m -> convert_structs (Map.find_exn st_map m.name)  m (includes ty_nss m mod_ns (module String))) ms in
    let fn_nss = prim_fn_ns :: (List.map ~f:(fun m -> (get_mid m.name, get_fn_ns m (includes ty_nss m mod_ns (module String)))) ms) in
    let () = List.iter ~f:(fun m -> (convert_fns (Map.find_exn st_map m.name) m
                                (includes ty_nss m mod_ns (module String)) 
                                (includes fn_nss m mod_ns (module String)) 
                                (includes (List.map ~f:(fun (i, sm) -> (get_mid i, sm.tys)) (Map.to_alist st_map)) m mod_ns (module Int))))
                                ms
    in
    let struct_map = Map.map st_map ~f:(fun d -> d.tys) in
    let fn_map = Map.map st_map ~f:(fun d -> d.fns) in
    let mods = (List.fold ms ~init:(Map.empty (module Int)) 
                           ~f:(fun acc m -> 
                            let mid = get_mid m.name in
                            Map.add_exn acc ~key:mid
                                            ~data:({ file = m.file
                                                   ; id = mid
                                                   ; fs = Map.find_exn fn_map m.name
                                                   ; ds = Map.find_exn struct_map m.name
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