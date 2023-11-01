# smlfut

`smlfut` allows SML programs to invoke Futhark programs via Futhark's
C API.  This is done by reading the manifest file emitted by the
Futhark compiler, then generating appropriate wrapper code.  For more
information see [the
manual](https://github.com/diku-dk/smlfut/releases/download/latest/smlfut.pdf).

## Installation

`smlfut` is written in SML.  By default the Makefile uses
[MLKit](https://github.com/melsman/mlkit), but `smlfut` can also be
compiled with [MLton](https://mlton.org).  Run

    make

to compile, or

    make install

to install.  Pass `MLCOMP=mlton` (or modify `config.mk`) to compile
with MLton.  By default the Makefile installs in `/usr/local`.  Pass a
different `PREFIX` to install elsewhere, e.g.:

    make install DESTDIR=$HOME/.local

## Testing

Run

    make run_test
