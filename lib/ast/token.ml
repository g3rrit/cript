
(*
type t = [
    | `ID of string
    | `INT of int
    | `STRING of string

    (* keywords *)
    | `LET
    | `DATA
    | `STRUCT
    | `OF

    (* operators *)
    | `EQ 
    | `PIPE
    | `SEMICOLON
    | `DOLLAR
    | `SPIPE
    | `OP of string

    (* parens *)
    | `LPAREN  (* ( *)
    | `RPAREN  (* ) *)
    | `LBRACE  (* { *)
    | `RBRACE  (* } *)
    | `LBRACK  (* [ *)
    | `RBRACK  (* ] *)

    | `EOF
]
*)