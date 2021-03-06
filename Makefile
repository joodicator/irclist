SRCDIR := .
OBJDIR := obj
BINDIR := bin

GHC := ghc --make
_GHCFLAGS := $(GHCFLAGS) -O -hidir $(OBJDIR) -odir $(OBJDIR)

all: bin/RawList bin/FormatList

bin/RawList: RawList.hs RawListLib.hs
	$(GHC) $(_GHCFLAGS) -o $@ $<

bin/FormatList: FormatList.hs Format.hs
	$(GHC) $(_GHCFLAGS) -o $@ $<

.phony: clean
clean:
	rm -rf {$(OBJDIR),$(BINDIR)}/*
