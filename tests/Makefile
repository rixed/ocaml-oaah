top_srcdir = ..

PROGRAMS = simple.opt font.opt
all: $(PROGRAMS)
opt: $(PROGRAMS:.byte=.opt)

ML_SOURCES = simple.ml
WITH_OWN_LIB = oaah

REQUIRES = algen graphics geom

include ../make.common

%.byte: %.cmo $(top_srcdir)/oaah.cma
	$(OCAMLC) -package "$(REQUIRES)" -I $(top_srcdir) oaah.cma -linkpkg $(OCAMLFLAGS) $< -o $@

%.opt: %.cmx $(top_srcdir)/oaah.cmxa
	$(OCAMLOPT) -package "$(REQUIRES)" -I $(top_srcdir) oaah.cmxa -linkpkg $(OCAMLOPTFLAGS) $< -o $@

clean-spec:

distclean-spec:

-include .depend
