include config.mk # Also contains install path.

all: smlfut

install: smlfut smlfut.1
	install -d $(PREFIX)/bin/
	install -m 644 smlfut ${PREFIX}/bin/
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

%/test: test/test.json %/test_main.sml %/test.sml
	$(MLTON) $(MLTONFLAGS) $*/test.mlb test/test.c $*/test.smlfut.c

run_test_poly: test_poly/test
	test_poly/test

run_test_mono: test_mono/test
	test_mono/test

run_test: run_test_poly run_test_mono

smlfut.pdf: smlfut.1
	groff -Tpdf -m mdoc ./smlfut.1 > smlfut.pdf

clean:
	find src test_poly test_mono -name MLB -exec rm -rf {} \;
	rm -rf MLB smlfut test/test.c test/test.h test/test.json test/test.sig test/test.sml test_*/*.c
