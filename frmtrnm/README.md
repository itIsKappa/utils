# `frmtrnm`

# Table of Contents
* [Manual](#manual)
  * [Usage](#usage)
  * [Options](#options)
* [Requirements](#requirements)

# Manual

## Usage
`frmtrnm <options> <files>`

## Options
* `-d`, `--delimiter=`\
  This accepts a string as the new delimiter[^Delimiter]. If unspecified,
  the default is a hyphen.

* `-s`, `--show`\
  Prints the set of bad characters to `stdout`. Further execution will be
  ceased and the program will exit.

* `-a`, `--add=`\
  This accepts a string of characters that will be *added* to the set of bad
  characters[^BadCharacters]. To see the characters that were added via the
  `show` option, the `show` option must succeed the `add` option. If the
  `show` option precedes the `add` option, no change will be seen, since the
  program would have already exited.
* `-x`, `--ignore-extension`\
  This switch will deny the existence of file extensions. Therefore, the
  file extension will become a regular part of the filename, and will be
  formatted as such.
  ```
  $ frmtrnm 'some.file file'
  <+> No formatting could be done (some.file file). Renaming skipped.
  1 selected; 0 renamed; 1 skipped.
  $ frmtrnm 'some.file file' -x
  <+> some.file file -> some.file-file
  1 selected; 1 renamed; 0 skipped.
  ```
* `-l`, `--min-cluster-limit=`\
  This option accepts an integer to define the minimum cluster limit of
  delimiters.  A cluster can contain one or more delimiters. Any cluster
  having less than the specified number of delimiters will be mutated to
  have the minimum number of delimiters.
  ```
  $ frmtrnm 'some  some.file' -l3
  <+> 'some some.file' to 'some---some.file'
  1 selected; 1 renamed; 0 skipped.
  ```
  It can be seen that three delimiters appeared for a double space.
* `-L`, `--max-cluster-limit=`\
  This option is identical to the above, except, as its name suggests, it is
  related to maximum cluster limits. Any cluster having more than the
  specified number of delimiters will be mutated to have the maximum number
  of delimiters.
  ```
  $ frmtrnm 'some  some.file' -L1
  <+> 'some  some.file' to 'some-some.file'
  1 selected; 1 renamed; 0 skipped.
  ```
  It can be seen that only a single delimiter appeared for the double space.

* `-h`, `--help`\
  Prints a help message.

# Requirements
| Type        | Name           | Comment                                 |
| ----------- | -------------- | :-------------------------------------: |
| Interpreter | CPython ≥3.9.2 | `frmtrnm` was tested with CPython 3.9.2 |
| Library     | `gnu_getopt`   | —                                       |


[^Delimiter]: The character with which the bad characters will be replaced
[^BadCharacters]: The characters that will be
  replaced with the delimiter.
