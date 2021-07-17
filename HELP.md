# `eudo`

`eudo <options> <command>`\
The above order must not be violated for a desired execution. This is since
every option (id est, that which begins with a hyphen) after the command
will be interpreted as the options for the command. The arguments to 
script will begin to be interpreted as the command from the first argument
that does begin with a hyphen. Exempli gratia,
`eudo -u some_user ls -harl`, will execute `ls -harl` as `some_user`.

# `frmtrnm`
`frmtrnm <options> <files>`\
Order of the passed arguments does not concern the program. Arguments are
parsed with GNU `getopt`, and as such will have its features. The following
are the available options.
* `-d`, `--delimiter=`\
This accepts a string as the new delimiter. If unspecified, the default is
a hyphen.
* `-s`, `--show`\
Prints the set of bad characters to `stdout`. Further execution will be ceased
and the program will exit.
* `-a`, `--add=`\
This accepts a string of characters that will be *added* to the set of bad
characters. To see the characters that were added via the `show` option, the
`show` option must succeed the `add` option. If the `show` option precedes
the `add` options, no change will be seen, since the program would have 
already exited.
* `-x`, `--ignore-extension`\
This switch will deny the existence of file extensions. Therefore, the file
extension will become a regular part of the filename, and will be
formatted as such.
* `-h`, `--help`\
Prints a help message.
