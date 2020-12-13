- variable namespace shadows function namespace (this is probably ok)
- fix block syntax
- add function expressions (later)

- int_(fn/struct)_map can be global state as their id is unique

- add void as primitive type (functions that return void should be treate specially)
- maybe just think about this and deal with it
- having a void types ads weird complications


- check if types of exp in jump are bool

- add condition to jmp and brk -> jmp [] brk [] | jmpc brkc  

#### Interfaces

struct obj {
    type_t* instance;
    void (*fn_1)(int a);
    ...
}

- pass object as pointer containing reference to static type struct implementing that interface