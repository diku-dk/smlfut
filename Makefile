MLKIT=mlkit
MLTON=mlton
MLTONFLAGS = \
  -default-ann 'allowFFI true'

all: smlfut

lib:
	smlpkg sync

smlfut: lib smlfut.mlb smlfut.sml
	$(MLKIT) -output $@ smlfut.mlb

test.json: test.fut
	futhark c --library test.fut

test.sml: test.json smlfut
	./smlfut test.json

test: test.json test_main.sml test.sml
	$(MLTON) $(MLTONFLAGS) test.mlb test.c

run_test: test
	./test
