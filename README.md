# enclose
Simple toy procedures for enclosing a string in brackets.

## Motivation
Some words or phrases are enclosed in symbols when composing messages for logging or error handling. For example, enclose array bounds in [] and variable names in ``. Since messages tend to be long, those are written in multiple lines using the concatenate operator as follows:

```Fortran
use :: stdlib_logger
use :: stdlib_strings
implicit none

real(real64), allocatable :: u(:, :)
character(:), allocatable :: variable_name

allocate (u(0:10, -1:9))

variable_name = "`u`"
call global_logger%log_information(variable_name//" allocation status "//to_string(allocated(u)))
call global_logger%log_information(variable_name//" range [["// &
                                   to_string(lbound(u, 1))//", "// &
                                   to_string(ubound(u, 1))//"], ["// &
                                   to_string(lbound(u, 2))//", "// &
                                   to_string(ubound(u, 2))//"]]")
```

Opening and closing symbols are sometimes placed on separate lines, making it difficult to find the pair brackets. It is also difficult to change the symbols, like `[ ]` to `(/ /)`.

The procedure `enclose` provided by this module encloses a string that passed as the argument. The procedure automatically determines the closing symbols corresponding to the opening symbols passed as the argument. It is, of course, possible to disable the auto-determination and specify the opening and closing symbols.

For example,

```Fortran
print *, enclose("string", "`")          ! "`string`"
print *, enclose("string", "[")          ! "[string]"
print *, enclose("string", "(/")         ! "(/string/)"
print *, enclose("string", "'([<{")      ! "'([<{string}>])'"
print *, enclose("string", "(", .false.) ! "(string("
print *, enclose("string", "(", "]")     ! "(string]"
```

## Usage
### Requirement
- A Fortran compiler
    - The library is tested using gfortran 10.3.0, intel fortran 2021.1, and nag fortran 7.1 on Windows 10.
- fpm
    - The library supports fpm (fortran-lang/fpm) for build. fpm 0.6.0 is used.
- FORD (optional)
    - FORD is used for generating the API document.

### Get the code
To get the code, execute the following commnad:

```
git clone https://github.com/degawa/enclose
cd enclose
```

### Build with fpm
To build the library using fpm, execute the following command:

```
fpm build
```

Then, install the library using:

```
fpm install --prefix path/to/your/libdir
```

### Module reference
Add the following `use` statement to procedures calling `enclose` or to the modules containing those:

```Fortran
use :: strings_enclose
```

Note that the module name is `strings_enclose`, not `enclose`.

### Reference as a fpm project's dependency
To use the enclose in your fpm project, add the following to `fpm.toml`.

```toml
[dependencies]
enclose = {git = "https://github.com/degawa/enclose"}
```

## API Document
The API documentation can be generated using FORD.

```console
ford api-doc-ford-settings.md
```
