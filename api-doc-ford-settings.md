---
output_dir: ./api-doc
src_dir: ./src
exclude_dir: ./test
             ./example
project: enclose
project_github: https://github.com/degawa/enclose
summary: Simple toy procedures for enclosing a string in brackets
author: Tomohiro Degawa
license: by-sa
docmark: !
docmark_alt: *
predocmark: >
predocmark_alt: |
display: public
         protected
         private
sort: permission-alpha
search: true
source: false
extra_mods: iso_fortran_env: https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
            testdrive: https://github.com/fortran-lang/test-drive
graph: false
coloured_edges: true
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
md_extensions: markdown.extensions.toc
               markdown.extensions.smarty
---

<!-- document's top page content --->
{!api-doc-index.md!}