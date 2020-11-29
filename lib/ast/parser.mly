%{
open Types
%}

%token <string> ID
%token <int> INT
%token <string> STRING

%token STRUCT
%token LET
%token FN
%token IF
%token THEN
%token ELSE
%token ELIF
%token RETURN
%token BEGIN
%token MODULE
%token END
%token JMP
%token BREAK
%token DEFER

%token EQ
%token PIPE
%token SPIPE
%token DOLLAR
%token BACKSLASH
%token COLON
%token SEMICOLON
%token COMMA
%token ARROW
%token HASH

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACK
%token RBRACK
%token EOF


%start <(string * string list * toplevel_t list)> entry

%type <toplevel_t> p_toplevel
%type <fn_t> p_fn
%type <struct_t> p_struct
%type <field_t> p_struct_field
%type <field_t> p_field
%type <type_t> p_type

%type <exp_prim_t> p_prim
%type <exp_t> p_exp_basic
%type <exp_t> p_exp_call
%type <exp_t> p_exp_app
%type <exp_t> p_exp_dollar
%type <exp_t> p_exp_spipe
%type <exp_t> p_exp

%type <stm_var_t> p_var
%type <stm_t> p_stm

%%

entry:
    | MODULE; n = ID; LBRACE; incs = list(ID); RBRACE; tls = list(p_toplevel); EOF { (n, "__prim" :: n :: incs, tls) }
    | MODULE; n = ID; tls = list(p_toplevel); EOF { (n, ["__prim"; n], tls) }

p_toplevel:
    | d = p_struct { Toplevel_struct d }
    | d = p_fn { Toplevel_fn d }

p_struct:
    | STRUCT; n = ID; LBRACE; f = list(p_struct_field) ; RBRACE { { sname = n; fs = f } }

p_struct_field:
    | f = p_field; SEMICOLON { f }

p_field:
    | n = ID; COLON; t = p_type { { name = n; ty = t }}

p_fn:
    | FN; n = ID; LPAREN; ags = separated_list(COMMA, p_field); RPAREN; ARROW; t = p_type; LBRACE; ss = list(p_stm); RBRACE
        { { fname = n; args = ags; ty = t; stms = ss }} 

p_type:
    | LPAREN; ags = separated_list(COMMA, p_type); RPAREN; ARROW; r = p_type { Type_fn (ags, r) }
    | i = ID { Type_prim i }

(* EXPRESSION *)

p_prim:
    | v = INT { Exp_prim_int v }
    | s = STRING { Exp_prim_string s }

p_exp_basic:
    | LPAREN; e = p_exp; RPAREN { e }
    | e = p_prim { Exp_prim e }
    | i = ID { Exp_ref i }

p_exp_call:
    | HASH; e = p_exp_basic { Exp_call e }
    | e = p_exp_basic { e }

p_exp_app:
    | l = p_exp_app; r = p_exp_call { Exp_app (l, r) }
    | e = p_exp_call { e }

p_exp_spipe:
    | l = p_exp_spipe; SPIPE; r = p_exp_app { Exp_app (r, l) }
    | e = p_exp_app { e }

p_exp_dollar:
    | l = p_exp_dollar; DOLLAR; r = p_exp_spipe { Exp_app (l, r) }
    | e = p_exp_spipe { e }

p_exp:
    | e = p_exp_dollar { e }

p_var:
    | n = ID; COLON; t = p_type; EQ; e = p_exp { { name = n; ty = t; v = e } }

p_stm:
    | e = p_exp; SEMICOLON { Stm_exp e }
    | v = p_var; SEMICOLON { Stm_let v }
    | RETURN; e = option(p_exp); SEMICOLON { Stm_return e }
    | JMP; n = option(ID); SEMICOLON { Stm_jump n}
    | BREAK; n = option(ID); SEMICOLON { Stm_break n}
    | BEGIN; n = option(ID); LBRACK; vs = separated_list(COMMA, p_var); RBRACK; me = option(p_exp); LBRACE; stms = list(p_stm); RBRACE { 
        Stm_scope (n, vs, me, stms) }
    | DEFER; LBRACE; stms = list(p_stm); RBRACE { Stm_defer stms }
    | s = p_struct { Stm_struct s }
    | f = p_fn { Stm_fn f }
