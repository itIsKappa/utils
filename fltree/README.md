# `fltree`
`fltree` is a command-line utility of convenience to generate visualization for
an arbitrary tree.  The tree can either be input as a file or directly into the
standard input.

## Table of Contents
* [Input Format](#input-format)
* [Manual](#manual)

## Input Format
`fltree` expects the input to be of a definite but simple format.  *Every child
of a parent must be indented more than the parent*.  There are no edge cases or
exceptions.

    Asia
      China
      India
      Singapore

The tree visualization for the above will be:

    Asia
    ├── China
    ├── India
    ╰── Singapore

Whereas

    Asia
      China
     India
        Singapore

will produce

    Asia
    ├── China
    ╰── India
        ╰── Singapore

The rule is hard and fast.  There are no exceptions.

## Manual
* `fltree -` \
  This will prompt the user to enter the input via standard input.  The input is
  read completely until EOF (`CTRL+D` in \*nix).  The result is promptly
  outputted to standard output.

* `fltree -f <filename>` \
  This will read *filename* and the result is outputted to standard output.
