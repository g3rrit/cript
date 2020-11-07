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

%token EQ
%token PIPE
%token SPIPE
%token DOLLAR
%token BACKSLASH
%token COLON
%token SEMICOLON
%token COMMA
%token ARROW

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LBRACK
%token RBRACK
%token EOF


%start <(string * string list * Types.Toplevel.t list)> entry

%type <Types.Toplevel.t> p_toplevel
%type <Types.Fn.t> p_fn
%type <Types.Struct.t> p_struct
%type <Types.Field.t> p_struct_field
%type <Types.Field.t> p_field
%type <Types.Type.t> p_type

%type <Types.Exp.prim> p_prim
%type <Types.Exp.t> p_exp_basic
%type <Types.Exp.t> p_exp_app
%type <Types.Exp.t> p_exp_dollar
%type <Types.Exp.t> p_exp_spipe
%type <Types.Exp.t> p_exp

%type <Types.Stm.var> p_var
%type <Types.Stm.t> p_stm

%%

entry:
    | MODULE; n = ID; LBRACE; incs = list(ID); RBRACE; tls = list(p_toplevel); EOF { (n, n :: incs, tls) }
    | MODULE; n = ID; tls = list(p_toplevel); EOF { (n, [n], tls) }

p_toplevel:
    | d = p_struct { Types.Toplevel.Struct d }
    | d = p_fn { Types.Toplevel.Fn d }

p_struct:
    | STRUCT; n = ID; LBRACE; f = list(p_struct_field) ; RBRACE { { name = n; fs = f } }

p_struct_field:
    | f = p_field; SEMICOLON { f }

p_field:
    | n = ID; COLON; t = p_type { { name = n; ty = t }}

p_fn:
    | FN; n = ID; LPAREN; ags = separated_list(COMMA, p_field); RPAREN; ARROW; t = p_type; LBRACE; ss = list(p_stm); RBRACE
        { { name = n; args = ags; ty = t; stms = ss }} 

p_type:
    | LPAREN; ags = separated_list(COMMA, p_type); RPAREN; ARROW; r = p_type { Types.Type.Fn (ags, r) }
    | i = ID { Types.Type.Prim i }

(* EXPRESSION *)

p_prim:
    | v = INT { Types.Exp.PInt v }
    | s = STRING { Types.Exp.PString s }

p_exp_basic:
    | LPAREN; e = p_exp; RPAREN { e }
    | e = p_prim { Types.Exp.Prim e }
    | i = ID; { Types.Exp.Ref i }

p_exp_app:
    | l = p_exp_app; r = p_exp_basic { Types.Exp.App (l, r) }
    | e = p_exp_basic { e }

p_exp_spipe:
    | l = p_exp_spipe; SPIPE; r = p_exp_app { Types.Exp.App (r, l) }
    | e = p_exp_app { e }

p_exp_dollar:
    | l = p_exp_dollar; DOLLAR; r = p_exp_spipe { Types.Exp.App (l, r) }
    | e = p_exp_spipe { e }

p_exp:
    | e = p_exp_dollar { e }

p_var:
    | n = ID; COLON; t = p_type; EQ; e = p_exp { { name = n; ty = t; v = e } }

p_stm:
    | e = p_exp; SEMICOLON { Types.Stm.Exp e }
    | RETURN; e = option(p_exp); SEMICOLON { Types.Stm.Return e }
    | v = p_var; SEMICOLON { Types.Stm.Let v }
    | n = option(ID); LBRACK; vs = separated_list(COMMA, p_var) RBRACK; e = option(p_exp); LBRACE; stms = list(p_stm); RBRACE { 
        Types.Stm.Scope (n, vs, e, stms) }
    | n = option(ID); e = option(exp); LBRACE; stms = list(p_stm); RBRACE {
        Types.Stm.Scope (n, [], e, stms) }