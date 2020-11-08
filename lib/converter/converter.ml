open Std
open Core
open Prim

open Namespace

module A = Ast.Types
module I = Ir.Types

module Prim = Prim

(* converter functions *)
let cident = ref 0

let next_ident () = let ii = !cident in cident := !cident + 1; ii

let init () = 
    cident := 0x1000 (* all prior symbols are reserved *)
    
let fold_struct (f : 'a -> A.Struct.t -> 'a) (a: 'a) (m : A.Module.t) : 'a = 
    List.fold m.tls ~init:a ~f:(fun aa tl -> (match tl with
            | A.Toplevel.Struct s -> f aa s
            | A.Toplevel.Fn _     -> aa
        ))

let fold_fn (f : 'a -> A.Fn.t -> 'a) (a: 'a) (m : A.Module.t) : 'a = 
    List.fold m.tls ~init:a ~f:(fun aa tl -> (match tl with
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
        | A.Exp.Call e        -> 
            let le = get_exp e ty_ns fn_ns var_ns structs in
            let t = I.Exp.get_type le in
            (match t with
                | I.Type.Fn ([], r) -> I.Exp.App (le, [], r) (* TODO: better error for not enough arguments *)
                | _ -> raise (Std.Error.Err "Cannot call variable of non function type"))
        | A.Exp.Access (l, r) -> 
            let find_ty_by_int i s =  (* this is probably the most inefficient function i have yet written *)
                match Map.filter ty_ns ~f:(fun a -> Int.equal a.id i) |> Map.data with
                    | [] -> raise (Std.Error.Comp "type that should be present not found")
                    | x :: _ -> Map.find_exn x.fs s
            in
            let le = get_exp l ty_ns fn_ns var_ns structs in
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


let get_var (v : A.Stm.var) (ty_ns : string_type_map) (fn_ns : string_fn_map) (var_ns : string_var_map) (structs : int_struct_map)
        : I.Stm.var =
    let lt = get_ty ty_ns v.ty in
    let ni = next_ident () in
    { id = ni
    ; ty = lt
    ; v = get_exp v.v ty_ns fn_ns var_ns structs
    }

let rec get_stm 
        (s : A.Stm.t) 
        (ty_ns : string_type_map) 
        (fn_ns : string_fn_map) 
        (var_ns : string_var_map) 
        (structs : int_struct_map) 
        (label_ns : string_int_map)
        : (I.Stm.t * string_var_map) =
    match s with
        | A.Stm.Scope (mn, vs, me, ss) -> 
            let ni = next_ident () in
            let nlabel_ns = Map.update label_ns (Option.value mn ~default:"__label") ~f:(fun _ -> ni) in
            let (vss, nvar_ns) = List.fold vs ~init:([], var_ns) 
                                   ~f:(fun acc a ->
                                        let (vs, var_ns) = acc in
                                        let v = get_var a ty_ns fn_ns var_ns structs in
                                        let nvar_ns = Map.update var_ns a.name ~f:(fun _ -> { id = v.id; ty = v.ty } ) in
                                        (v :: vs, nvar_ns))
            in
            let ne = Option.map me ~f:(fun a -> get_exp a ty_ns fn_ns nvar_ns structs) in
            let nss = get_stm_list ss ty_ns fn_ns nvar_ns structs nlabel_ns in
            (I.Stm.Scope (ni, List.rev vss, ne, nss), var_ns)
        | A.Stm.Let v -> 
            let lt = get_ty ty_ns v.ty in
            let ni = next_ident () in
            let nvar_ns = Map.update var_ns v.name ~f:(fun _ -> {id = ni; ty = lt }) in
            (I.Stm.Let ({ id = ni; ty = lt; v = get_exp v.v ty_ns fn_ns var_ns structs }), nvar_ns)
        | A.Stm.Exp e         -> (I.Stm.Exp (get_exp e ty_ns fn_ns var_ns structs), var_ns)
        | A.Stm.Return me     -> (I.Stm.Return (Option.map me ~f:(fun a -> get_exp a ty_ns fn_ns var_ns structs)), var_ns)
        | A.Stm.Jump mn       -> 
            let v = Map.find_exn label_ns (Option.value mn ~default:"__label") in
            (I.Stm.Jump v, var_ns)
        | A.Stm.Break mn       -> 
            let v = Map.find_exn label_ns (Option.value mn ~default:"__label") in
            (I.Stm.Break v, var_ns)

and get_stm_list 
        (ss: A.Stm.t list)
        (ty_ns  : string_type_map)
        (fn_ns : string_fn_map)
        (var_ns : string_var_map)
        (structs : int_struct_map)
        (label_ns : string_int_map)
        : I.Stm.t list =
    let (r, _) = List.fold ss ~init:([], var_ns) (* TODO: maybe fold other way *)
                              ~f:(fun acc a ->
                                    let (ss, var_ns) = acc in
                                    let (s, nvar_ns) = (get_stm a ty_ns fn_ns var_ns structs label_ns) in
                                    (s :: ss, nvar_ns))
    in List.rev r


let get_fn (m : A.Module.t) (ty_ns : string_type_map) (fn_ns : string_fn_map) (structs : int_struct_map): int_fn_map =
    fold_fn (fun ff f -> 
        let fns = Map.find_exn fn_ns f.name in
        let var_ns = get_args_ns f.args ty_ns in
        Map.add_exn ff ~key:fns.id
                       ~data:({ id = fns.id
                             ; args = List.map f.args ~f:(fun a -> get_field a var_ns)
                             ; ty = (let (_, r) = fns.ty in r)
                             ; stms = get_stm_list f.stms ty_ns fn_ns var_ns structs (Map.empty (module String))
                             ; tags = if f.name == "main" then [I.Fn.Main] else []
                             (*let (s, _) = get_stm f.stm ty_ns fn_ns var_ns structs in s*)
                             } : I.Fn.t)
    ) (Map.empty (module Int)) m

let get_mod_ns (ms : A.Module.t list) : string_int_map = 
    List.fold ms ~init:(Map.singleton (module String) "__prim" 0) ~f:(fun acc a -> Map.add_exn acc ~key:a.name ~data:(next_ident ()))

let includes (xs : (int * ('a, 'v, 'cmp) Map.t) list) (m : A.Module.t) (mod_ns : string_int_map) (comp : ('a, 'cmp) Map.comparator) : ('a, 'v, 'cmp) Map.t =
    let mids = List.map m.incs ~f:(fun a -> Map.find_exn mod_ns a) in
    let fs   = List.filter xs ~f:(function (a, _) -> List.exists mids ~f:(fun b -> Int.equal a b)) in
    let fss  = List.map fs ~f:(function (_, a) -> a) in
    Std.Map.merge_list comp fss (fun ~key:_ a _ -> a) 

let convert (ms : A.Module.t list) : I.Unit.t = (* this is all so inefficient *)
    let mod_ns = get_mod_ns ms in
    let get_mid k = Map.find_exn mod_ns k in
    let ty_nss = prim_ty_ns :: (List.map ~f:(fun m -> (get_mid m.name, get_ty_ns m)) ms) in
    let structss = List.map ~f:(fun m -> (get_mid m.name, get_struct m (includes ty_nss m mod_ns (module String)))) ms in
    let fn_nss = prim_fn_ns :: (List.map ~f:(fun m -> (get_mid m.name, get_fn_ns m (includes ty_nss m mod_ns (module String)))) ms) in
    let fnss = List.map ~f:(fun m -> (get_mid m.name, get_fn m 
                                (includes ty_nss m mod_ns (module String)) 
                                (includes fn_nss m mod_ns (module String)) 
                                (includes structss m mod_ns (module Int))))
                                ms
    in
    let struct_map = Map.of_alist_exn (module Int) structss in
    let fn_map = Map.of_alist_exn (module Int) fnss in 
    let mods = (List.fold ms ~init:(Map.empty (module Int)) 
                           ~f:(fun acc m -> 
                            let mid = get_mid m.name in
                            Map.add_exn acc ~key:mid
                                            ~data:({ file = m.file
                                                   ; id = mid
                                                   ; fs = Map.find_exn fn_map mid
                                                   ; ds = Map.find_exn struct_map mid
                                                   ; incs = List.map m.incs ~f:get_mid
                                                   } : I.Module.t)))
    in
    let main_mod = List.find (Map.data mods) ~f:(fun m -> 
        Map.exists m.fs ~f:(fun f -> List.mem I.Fn.Main f.tags 
                                ~compare:(fun a _ -> (match a with | Main -> true | _ -> false))))
    in
    { mods = mods
    ; main = main
    }