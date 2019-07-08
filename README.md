# LispyCpp
***A rewrote version of Lispy in C++17***

## Origins
The [Build Your Own Lisp](http://www.buildyourownlisp.com "Build Your Own Lisp") project
is a very good tutorial for those who want to use C to write a compiler.
The project's final result is a working toy compiler for Lispy, a version of simplified Lisp.

However, the project is wrote in C99, a simple but old language.
So I decided to rewrote the whole project using C++17

## Build Requirements
- a C++17-compliant compiler
- a copy of [mpc](https://github.com/orangeduck/mpc "mpc")
  (I have included one in the project)
- [CMake](https://cmake.org "CMake") (optional, but recommended)

## Build
### Build using CMake (recommended)
    git clone https://github.com/Mick235711/LispyCpp.git
    cd LispyCpp
    mkdir build
    cd build
    cmake ..
    make
### Build without CMake
    g++ -std=c++17 -o LispyCpp main.cpp mpc.c
    
## Used C++ Features
`C++98/03`:
- use `std::map` and `std::vector` instead of raw arrays
to store `lval`s and symbol table
- use `std::string` instead of `char*` to store strings
- replace pass-by-pointer by pass-by-(const)-references
- change all `lval_*` constructors into real `lval` constructors
  (uses tag-dispatch to differ Errors, Symbols and Strings)
- change `lval_print(ln)` into `operator<<`
- differ Functions and Lambdas

`C++11`:
- use `auto` for variable definition
- use `static_cast` instead of C-style casts
- use `std::shared_ptr` and `std::unique_ptr` instead of raw pointers
  (also used for managing `mpc_*_t` objects)
- use `std::function` to store functions
- use lambdas to prevent wrapper functions
- use user-defined literals(`operator""s` for std::string)
- use raw strings(`R("")`) to declare regular expressions
- use `noexcept` to all functions

`C++14`:
- use `auto`-deduced function return type

`C++17`:
- use `std::variant` to save storage
- change members to member functions for access control

## Language
### Basic types
There are **eight** types in Lispy: `Number`, `Symbol`, `String`, `Error`, `Q-Expression`, `S-Expression`, `Function`, `Lambda`  
Type definition in mpc language: (/x/ implies that x is a regex)

    number  : /(+|-)?[0-9]+/ ;
    symbol  : /[a-zA-Z0-9_+\-*\/\\=<>!&]+/ ;
    string  : /"(\\.|[^"])*"/ ;
    comment : /;[^\r\n]*/ ;
    sexpr   : '(' <expr>* ')' ;
    qexpr   : '{' <expr>* '}' ;
    expr    : <number> | <symbol> | <string> | <comment> | <sexpr> | <qexpr> ;
    lispy   : /^/ <expr>* /$/ ;

### Builtin functions
Builtin functions in Lispy are `list`, `head`, `eval`, `tail`, `join`,
`def`, `=`, `\\`(lambda), `+`, `-`, `*`, `/`, `<`, `>`, `<=`, `>=`, `==`, `!=`,
`if`, `load`, `print` and `error`. All other functions are defined by user.

### Standard Library
This project had included a library suggested by Chapter 15 of Build Your Own Lisp
in `prelude.lspy`.
You can simply `load` it in a Lispy prompt/file.  
Many useful function like `fun`, `map`, `foldl` are included.

## Future
There are still a lot to invent for this toy Lispy compiler and language.
#### Todo for present time
Finish most of the bonus questions in Build Your Own Lisp, like adding logical operators.
#### Future Todo
Pick a few Bonus Projects in Chapter 16 to finish, like float-number and user-defined types.
