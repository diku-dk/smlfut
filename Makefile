all: futhark-sml

lib:
	smlpkg sync

futhark-sml: lib futhark-sml.mlb *.sml
	mlkit -output $@ futhark-sml.mlb

