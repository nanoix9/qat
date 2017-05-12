# undecied

- can anonymous function be multi dispatched? Seems OK, like

        foo = func ...
        func ... implements foo

- type can be a function instead of a special form

- can `import` be a function?

- can a module partly implemented in host language (OCaml) or destination language (Qat)?

    No, but can make two modules `foo` and `fooext`, and extend functions that defined in `foo`
    in `fooext` with Qat

# TODO

- `getattr` & `setattr` for module access, like `foo.bar` is `getattr foo bar`

- implement `goto`

- implement defining macro

- print stack trace when exception occurs
