- variable namespace shadows function namespace (this is probably ok)
- fix block syntax
- add function expressions


#### Interfaces

struct obj {
    type_t* instance;
    void (*fn_1)(int a);
    ...
}

- pass object as pointer containing reference to static type struct implementing that interface