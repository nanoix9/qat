# undecied

- can anonymous function be multi dispatched? Seems OK, like

        foo = func ...
        func ... implements foo

- type can be a function instead of a special form

- can `import` be a function?

- can a module partly implemented in host language (OCaml) or destination language (Qat)?

    No, but can make two modules `foo` and `fooext`, and extend functions that defined in `foo`
    in `fooext` with Qat

- in function definition

        func foo(a :int, b :string) (...)

    while in function application

        foo(x :int, "foo")

    which means actually

        (foo (cast x int) "foo")



# TODO

- `getattr` & `setattr` for module access, like `foo.bar` is `getattr foo bar`

- implement `goto`

- implement defining macro

- print stack trace when exception occurs

- *Important* subtyping checking of function implementations: forbid the "crossing"

    e.g. if there are two implementations of a function "foo":

        foo: a -> b -> c
        foo: x -> y -> z

    if x is a subtype of a, i.e. x <: a, then y satisfies

        y <: a or y = a


