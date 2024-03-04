include config.mk # Also contains install path.

all: smlfut

install: smlfut smlfut.1
	install -d $(PREFIX)/bin/
	install -m 755 smlfut ${PREFIX}/bin/
	install -d $(MANPREFIX)/man1
	install -m 644 smlfut.1 ${MANPREFIX}/man1/

smlfut: src/smlfut.mlb src/*.sml
	$(MLCOMP) -output $@ src/smlfut.mlb

test/test.json: test/test.fut
	$(FUTHARK) $(FUTHARK_BACKEND) --library $<

test_poly/test.sml: test/test.json smlfut
	./smlfut --poly-arrays test/test.json -o test_poly

test_mono/test.sml: test/test.json smlfut
	./smlfut --mono-arrays test/test.json -o test_mono

test_mlkit/test.sml: test/test.json smlfut
	./smlfut --target=mlkit test/test.json -o test_mlkit

test_mlkit/test: test/test.json test_mlkit/test_main.sml test_mlkit/test.sml
	cd test_mlkit && gcc -c ../test/test.c test.smlfut.c
	cd test_mlkit && ar r libtest.a test.o test.smlfut.o
	$(MLKIT) -o test_mlkit/test -libdirs test_mlkit -libs "m,c,dl,test" -no_gc test_mlkit/test.mlb

%/test: test/test.json %/test_main.sml %/test.sml
	$(MLTON) $(MLTONFLAGS) $*/test.mlb test/test.c $*/test.smlfut.c

run_test_poly: test_poly/test
	cd test_poly && ./test

run_test_mono: test_mono/test
	cd test_mono && ./test

run_test_mlkit: test_mlkit/test
	cd test_mlkit && ./test

# mlkit temporarily disabled as there is some codegen bug.
run_test: run_test_poly run_test_mono # run_test_mlkit

smlfut.pdf: smlfut.1
	groff -Tpdf -m mdoc ./smlfut.1 > smlfut.pdf

clean:
	find src test_poly test_mono test_mlkit -name MLB -exec rm -rf {} \;
	rm -rf MLB smlfut test/test.c test/test.h test/test.json test/test.sig test/test.sml test_*/*.c
