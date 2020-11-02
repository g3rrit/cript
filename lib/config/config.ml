
let version_major = "0"
let version_minor = "0"
let version_patch = "0"

let version 
    = String.concat "." [version_major; version_minor; version_patch]

let cc = ref "clang"
let cc_set =
    [ "clang"; "gcc" ]
let set_cc c 
    = if List.exists (fun s -> c = s) cc_set 
      then cc := c 
      else (Stdio.printf "Invalid compiler (%s)\n" c ; exit 0)

let to_string ()
    = String.concat "\n"
    [ "CONFIG"
    ; "VERSION: " ^ version 
    ; "C COMPILER: " ^  !cc
    ]