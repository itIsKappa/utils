# `frmtrnm`
`frmtrnm <options> <files>`\

* `-d`, `--delimiter=`\
This accepts a string as the new delimiter. If unspecified, the default is
a hyphen.

* `-s`, `--show`\
Prints the set of bad characters to `stdout`. Further execution will be ceased
and the program will exit.

* `-a`, `--add=`\
  This accepts a string of characters that will be *added* to the set of bad
  characters. To see the characters that were added via the `show` option,
  the `show` option must succeed the `add` option. If the `show` option
  precedes the `add` option, no change will be seen, since the program would
  have already exited.
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


# `bkmk`
`bkmk <verb>`\
`<verb>` is followed by its respective arguments.  If `bkmk` is invoked
without a verb specified, the behaviour is identical to `bkmk view`.

* `add <book name> <page number>`\
  This adds a bookmark and assigns it a number for ease of later reference. 
  A bookmark may not necessarily be assigned a chronologically sequential
  number.
  ```
  $ bkmk add 'Song of Solomon' 69
  Added: ‘Song of Solomon’ pp.69
  ```
* `update <book number> <new page number>`\
  This updates the page number of the bookmark referred to by `<book number>`.
  ```
  $ bkmk update 0 70
  ‘Song of Solomon’: 69 ⇒ 70
  ```
* `view`\
  This displays a table of the stored bookmarks and is the behaviour implied
  in the absence of a verb.
  ```
  ┌─────┬─────────────────┬──────────────┐
  │   # │ Name            │ Page number  │
  ├─────┼─────────────────┼──────────────┤
  │   0 │ Song of Solomon │ 69           │
  └─────┴─────────────────┴──────────────┘
  ```
* `remove <book number>`\
  This removes the bookmark referred to by `<book number>`.
  ```
  $ bkmk remove 0
  Removed: ‘Song of Solomon’
  ```
* `help`\
  This displays a quick help for the user.

Also, the first letter of each verb is its alias — i.e. a verb can be referred
to by its first letter instead of the full word.
