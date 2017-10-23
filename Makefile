BUILDDIR ?= build
CFG      ?= debug # debug, release
NAME     ?= c4
SRCDIR   ?= src

all:

# Be verbose about the build.
Q ?= @

BINDIR := $(BUILDDIR)/$(CFG)
BIN    := $(BINDIR)/$(NAME)

# not sure if we need this / if this works with stack

ifneq ("$(wildcard llvm/install/bin/llvm-config)","")
	LLVM_CONFIG  ?= llvm/install/bin/llvm-config
else
	LLVM_CONFIG  ?= llvm-config
endif


.PHONY: all clean

setup:
	stack setup

all: setup
	stack build
	mkdir -p $(BINDIR)
	cp `stack path --dist-dir`/build/c4/c4 $(BINDIR)

run:
	stack exec -- c4

clean:
	@echo "===> CLEAN"
	stack clean
	$(Q)rm -fr $(BINDIR)
