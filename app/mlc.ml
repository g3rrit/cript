open Pipe
open Stdio

let arg_spec
    = [ ( "-c"
        , Arg.String (fun s -> Config.set_cc s)
        , "c compiler used"
        )
      ]

let usage_msg
    = "Usage: cx [options] file..."

let () 
    = printf "%s\n" "----- C0 LANG -----"
    ; let files = ref [] in
      Arg.parse arg_spec (fun s -> files := (String.split_on_char '/' s) :: !files) usage_msg
    ; run !files