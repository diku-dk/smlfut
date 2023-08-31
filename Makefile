MLKIT=mlkit
FUTHARK=futhark
FUTHARK_BACKEND=c
MLTON=mlton

MLTONFLAGS = \
  -default-ann 'allowFFI true'

ifeq ($(FUTHARK_BACKEND),hip)
MLTONFLAGS += -link-opt '-lhiprtc -lamdhip64'
endif

ifeq ($(FUTHARK_BACKEND),opencl)
MLTONFLAGS += -link-opt '-lOpenCL'
endif

ifeq ($(FUTHARK_BACKEND),cuda)
MLTONFLAGS += -link-opt '-lcuda -lnvrtc -lcudart'
endif

all: smlfut

lib:
	smlpkg sync

smlfut: lib smlfut.mlb smlfut.sml
	$(MLKIT) -output $@ smlfut.mlb

test.json: test.fut
	$(FUTHARK) $(FUTHARK_BACKEND) --library test.fut

test.sml: test.json smlfut
	./smlfut test.json

test: test.json test_main.sml test.sml
	$(MLTON) $(MLTONFLAGS) test.mlb test.c

run_test: test
	./test
