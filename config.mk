PREFIX ?= /usr/local
MANPREFIX ?= ${PREFIX}/share/man

MLKIT=mlkit
FUTHARK=futhark
FUTHARK_BACKEND=c
MLTON=mlton

MLCOMP ?= $(MLKIT)

MLTONFLAGS = \
	-default-ann 'allowFFI true' \
	-codegen c \
	-default-type int64

ifeq ($(FUTHARK_BACKEND),hip)
MLTONFLAGS += -link-opt '-lhiprtc -lamdhip64'
endif

ifeq ($(FUTHARK_BACKEND),opencl)
MLTONFLAGS += -link-opt '-lOpenCL'
endif

ifeq ($(FUTHARK_BACKEND),cuda)
MLTONFLAGS += -link-opt '-lcuda -lnvrtc -lcudart'
endif
