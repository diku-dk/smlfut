# smlfut

`smlfut` allows SML programs to invoke Futhark programs via Futhark's
C API.  This is done by reading the manifest file emitted by the
Futhark compiler, then generating appropriate wrapper code.  For more
information see [the
manual](https://github.com/diku-dk/smlfut/releases/download/latest/smlfut.html).

## Installation

`smlfut` is written in SML.  The Makefile currently assumes
[MLkit](https://github.com/melsman/mlkit), but it is probably easy to
support other SML compilers.  Run

    make

to compile, or

    make install

to install.  By default, this installs in `/usr/local`.  Pass a
different `PREFIX` to install elsewhere, e.g.:

    make install DESTDIR=$HOME/.local

## Testing

Run

    make run_test
