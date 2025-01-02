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

## Wood Types
*Wood types* refer to the set of box-drawing characters used for
pretty-printing.  There are three types:
* `ASCII` \
  ASCII characters may look rather dull and not very legible.  However this is
  the most compatible, and is guaranteed to work anywhere.

* `Unicode` \
  Unicode characters are legible and look nice.  They do not have abrupt breaks
  between characters (unless the typeface chosen does or the line height is
  greater) unlike ASCII characters.  It is, however, not guaranteed work
  everywhere, since some systems may not have typefaces having box-drawing
  characters or may not (unfortunately) support UTF-8 characters.

* `Round` \
  Round characters are like `Unicode`, except that the corners are rounded.  A
  system supporting the usual box-drawing characters may not necessarily support
  rounded box-drawing characters — this does not hold the other way around,
  usually.

The wood type is specified by the `-d` option, as like `-d Round`.  `Unicode` is
the default wood type.  If an inexistent wood type is specified, the wood type
defaults to `Unicode`. 

## Manual
* `fltree -` \
  This will prompt the user to enter the input via standard input.  The input is
  read completely until EOF (`CTRL+D` in \*nix).  The result is promptly
  outputted to standard output.

* `fltree -f <filename>` \
  This will read *filename* and the result is outputted to standard output.

## Building
* `cabal build`\
  This will compile and build `fltree`.

* `cabal run`\
  This will compile and build, if not already, and subsequently run the program.
  The command-line arguments may be provided like so: `cabal run -- -` or `cabal
  run -- -f <filename>`.
