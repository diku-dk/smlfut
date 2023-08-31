all: smlfut

lib:
	smlpkg sync

smlfut: lib smlfut.mlb *.sml
	mlkit -output $@ smlfut.mlb

