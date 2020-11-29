module main


struct foo_t { 
    b : int; 
    a : () -> int;
    c : (int, int) -> int;
}

struct bar_t {
    a : int;
}

fn vtest() -> int { 

    begin a [ i : int = 111 ] {
        return i;
    }

    return 01; 
}

fn main (argc : int, argv : int) -> int {

    a : int = 2;

    fn test(a : int) -> int {
        #print_int a;
        return 10;
    }

    b : int = 3;
    c : int = 4;

    defer {
        #print_int c;
    }

    #print_int a;

    #test b;

    begin a [ i : int = 123 ] {
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