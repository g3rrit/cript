open Core

type string_int_map = (string, int, String.comparator_witness) Map.t

let string_int_map_show (m : string_int_map) : string  =
    Map.fold m ~init:"; " ~f:(fun ~key:k ~data:v a -> String.concat [a ; k; " -> "; string_of_int v; "; "])

module Map = struct
    include Map
    
    let merge_list (cmp : (('a, 'cmp) Core.Map.comparator)) (lm : ((('k, 'v, 'cmp) t) list)) (combine : (key:'k -> 'v -> 'v -> 'v)) : ('k, 'v, 'cmp) t =
        List.fold lm ~init:(Map.empty cmp) ~f:(fun m a -> Map.merge_skewed m a ~combine:(combine))
end

module File = struct
    type t = string list
    [@@deriving show]

    let to_string (s : t) : string = 
        String.concat ~sep:"/" s
end

module Location = struct 
    type pos = 
        { line : int 
        ; col  : int
        }

    type loc = pos * pos

    let (<:>) ((l, _) : loc) ((_, r) : loc) : loc =
        (l, r)

end

module Error = struct 
    type kind = 
        | LexerError
        | ParserError

    type t = 
        { src : File.t 
        ; ek  : kind
        ; loc : Location.loc 
        ; msg : string
        }

    exception E of t

    exception Comp of string

    exception Err of string

    let to_string (e : t) : string =
        let sl = (fst e.loc).line in
        let el = (snd e.loc).line in
        let sc = (fst e.loc).col in
        let ec = (snd e.loc).col in
        let lines = File.to_string e.src 
            |> Stdio.In_channel.read_lines 
            |> List.sub ~pos:(sl - 1) ~len:(el - sl + 1)
            |> List.fold ~init:(sl, "") ~f:(fun a l -> let (c, ol) = a in 
                (c + 1, Printf.sprintf "%s%6d | %s\n" ol c l))
            |> snd in
        let err_line = 
            [ List.init ((min sc ec) + 8) ~f:(const " ")
            ; List.init (Int.abs @@ sc - ec) ~f:(const "^") 
            ] |> List.concat
            |> String.concat in
        let sep = "------------------------------" in

        Printf.sprintf 
            "Error in %s at (%d, %d):\n%s\n%s%s\n%s\n[%s]\n" 
            (File.to_string e.src) 
            ((fst e.loc).line) 
            ((fst e.loc).col) 
            sep
            lines
            err_line
            sep
            e.msg

    let _line_to_string (l : int) (s : string) : string =
        Printf.sprintf "%6d | %s\n" l s

    let print (e : t) : unit = 
        Printf.printf "%s" @@ to_string e; ()
    
    let handle : exn -> 'a = function
        | E e    -> print e; exit (-1)
        | Comp e -> Printf.printf "COMPILER ERROR (%s)" e
        | Err e  -> Printf.printf "ERROR (%s)" e
        | e      -> Printf.printf "UNKNOWN COMPILER ERROR\n"; raise e

end

