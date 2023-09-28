include config.mk # Also contains install path.

all: smlfut

install: smlfut smlfut.1
	@echo \# Installing executable files to ${PREFIX}/bin
	@mkdir -p ${PREFIX}/bin/
	install smlfut ${PREFIX}/bin/
	@echo \# Installing manual page to ${MANPREFIX}/man1
	@mkdir -p ${MANPREFIX}/man1/
	@echo \# Installing manpages to ${MANPREFIX}/man1/
	install -D -m 644 smlfut.1 ${MANPREFIX}/man1/


smlfut: src/smlfut.mlb src/*.sml
	$(MLCOMP) -output $@ src/smlfut.mlb

test/test.json: test/test.fut
	$(FUTHARK) $(FUTHARK_BACKEND) --library $<

test_poly/test.sml: test/test.json smlfut
	./smlfut --poly-arrays test/test.json -o test_poly

test_mono/test.sml: test/test.json smlfut
	./smlfut --mono-arrays test/test.json -o test_mono

%/test: %/test.json %/test_main.sml %/test.sml
	$(MLTON) $(MLTONFLAGS) $*/test.mlb $*/test.c $*/test.smlfut.c

run_poly_test: test_poly/test
	test_poly/test

run_mono_test: test_mono/test
	test_mono/test

run_test: run_poly_test run_mono_test

clean:
	find src test_poly test_mono -name MLB -exec rm -rf {} \;
	rm -rf MLB smlfut test/test.c test/test.h test/test.json test/test.sig test/test.sml
