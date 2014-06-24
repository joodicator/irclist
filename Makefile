SRCDIR := .
OBJDIR := obj
BINDIR := bin

GHC := ghc --make
_GHCFLAGS := $(GHCFLAGS) -O -hidir $(OBJDIR) -odir $(OBJDIR)

bin/Main: Main.hs IRCList.hs
	$(GHC) $(_GHCFLAGS) -o $@ $<

.phony: clean
clean:
	rm -rf {$(OBJDIR),$(BINDIR)}/*
