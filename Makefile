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

test/test.sml: test/test.json smlfut
	./smlfut test/test.json

test/test: test/test.json test/test_main.sml test/test.sml
	$(MLTON) $(MLTONFLAGS) test/test.mlb test/test.c test/test.smlfut.c

run_test: test/test
	test/test

clean:
	find src test -name MLB -exec rm -rf {} \;
	rm -rf MLB smlfut test/test.c test/test.h test/test.json test/test.sig test/test.sml
