# For more readable S-expression:

- dwheeler

    <https://sourceforge.net/p/readable/wiki/Home/>
    curly infix expression  // { 1 + 2 }
    sweet expression  // use indent

- M-expression

- P4P

    <http://shriram.github.io/p4p/#(part._.Adopting_.Indentation_.Without_.Semantics)>

- PLOT language

    <http://users.rcn.com/david-moon/PLOT/index.html>

- O-expr

    <http://lambda-the-ultimate.org/node/4879>
    <http://breuleux.net/blog/liso.html>

- Dylan

    Implement Lisp macro power but with Algol-like grammar

    <https://opendylan.org/books/drm/Rewrite_Rule_Examples>

# Mixfix operator parsing

- Parsing Mixfix Operators

    @inproceedings{danielsson2008parsing,
      title={Parsing mixfix operators},
      author={Danielsson, Nils Anders and Norell, Ulf},
      booktitle={Symposium on Implementation and Application of Functional Languages},
      pages={80--99},
      year={2008},
      organization={Springer}
    }

- Parsing Mixfix Expressions

    @article{wieland2010parsing,
      title={Parsing Mixfix Expressions. Dealing with User-Defined Mixfix Operators Efficiently},
      author={Wieland, Jacob},
      year={2010}
    }

- There have been many attempts since LISP's creation to give the language a rich ALGOL-like syntax, including Pratt's CGOL, LISP 2, MLISP, Dylan, Interlisp's Clisp, and McCarthy's original M-expressions. All failed to find acceptance.

    <http://javascript.crockford.com/tdop/tdop.html>

- C+++: User-Defined Operator Symbols in C++

    @article{heinlein2004c+++,
      title={C+++: User-Defined Operator Symbols in C++.},
      author={Heinlein, Christian},
      journal={GI Jahrestagung (2)},
      volume={51},
      pages={459--468},
      year={2004},
      publisher={Citeseer}
    }

# Earley Parser

- Loup

    <http://loup-vaillant.fr/tutorials/earley-parsing/recogniser>

# Memory model in virtual machine

## Python

- <https://jakevdp.github.io/blog/2014/05/09/why-python-is-slow/>

        a->PyObject_HEAD->typecode
        a->val = 1

        a->PyObject_HEAD is int (so is b)
        so call binary_add<int, int>(a->val, b->val)

- <http://yanyahua.com/2016/9/1/python%E4%B8%ADload_attr%E5%92%8Cstore_attr%E6%8C%87%E4%BB%A4>

    LOAD_ATTR

        PyObject_GetAttr(PyObject *v, PyObject *name){
           PyObject_GetAttr(PyObject *v, PyObject *name)
                return (*tp->tp_getattro)(v, name);
        }

    STORE_ATTR

        PyObject_SetAttr(PyObject *v, PyObject *name, PyObject *value){
            PyTypeObject *tp = Py_TYPE(v);
                return (*tp->tp_setattro)(v, name, value);
        }

    而所有内置PyTypeObject的tp_setattro和tp_getattro都是(极少数例外)

        PyObject_GenericGetAttr,                    /* tp_getattro */
        PyObject_GenericSetAttr,                     /* tp_setattro */

