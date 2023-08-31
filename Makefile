all: smlfut

lib:
	smlpkg sync

smlfut: lib smlfut.mlb *.sml
	mlkit -output $@ smlfut.mlb

test.json: test.fut
	futhark c --library test.fut

test: smlfut test.json
	./smlfut test.json
	mlkit -output test test.mlb

run_test: test
	./test
