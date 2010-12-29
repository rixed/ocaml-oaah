OCAMLPATH = ../..

PROGRAMS = simple.byte font.byte
all: $(PROGRAMS)
opt: $(PROGRAMS:.byte=.opt)

ML_SOURCES = simple.ml

REQUIRES = oaah geom

include ../make.common

.cmo.byte:
	$(OCAMLC)   -o $@ -package "$(REQUIRES)" -linkpkg $(OCAMLFLAGS) $^

.cmx.opt:
	$(OCAMLOPT) -o $@ -package "$(REQUIRES)" -linkpkg $(OCAMLOPTFLAGS) $^

-include .depend
