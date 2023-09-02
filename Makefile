include config.mk

all: smlfut

src/lib:
	cd src && smlpkg sync

smlfut: src/lib src/smlfut.mlb src/*.sml
	$(MLKIT) -output $@ src/smlfut.mlb

test/test.json: test/test.fut
	$(FUTHARK) $(FUTHARK_BACKEND) --library $<

test/test.sml: test/test.json smlfut
	./smlfut test/test.json

test/test: test/test.json test/test_main.sml test/test.sml
	$(MLTON) $(MLTONFLAGS) test/test.mlb test/test.c

run_test: test/test
	test/test

clean:
	rm -rf smlfut MLB src/MLBtest/MLB test/test.c test/test.h test/test.json test/test.sig test/test.sml
