module main


struct foo_t { 
    b : int; 
    a : () -> int;
    c : (int, int) -> int;
}

struct bar_t {
    a : int;
}

fn vtest() -> void {}

fn main (argc : int, argv : int) -> int {

    a : int = 2;

    fn test(a : int) -> void {
        #print_int a;
    }

    b : int = 3;

    #print_int a;

    #test b;

    begin a [] {
        return 0;
    }

    begin a [ i : int = 112310 ] i {
        jmp a;
        brk a;
    }
    
    return 0;
}


fn lel(a : int, b : int) -> () -> (int) -> int {
    return 0;
}