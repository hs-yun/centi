# centi

A very simple text editor that currently supports:
  + incremental search (`ctrl-f`, arrow keys to navigate)
  + syntax highlighting for C.

## Compilation
```bash
$ make
```
with the included Makefile. No dependencies.

## Execution
```bash
$ ./centi [filename]
```

## License
The editor was written with modifications from Paige Ruten ([snaptoken](https://github.com/snaptoken))'s excellent tutorial, which in turn was based off Salvatore Sanfilippo ([antirez](https://github.com/antirez))'s source code for `kilo`. `kilo` was released under the [BSD 2-clause](https://choosealicense.com/licenses/bsd-2-clause/) license, and all unmodified portions of `kilo` source code are copyright (C) 2016 Salvatore Sanfilippo.

`centi` is licensed under [GNU GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html).
