top_srcdir = ..

PROGRAMS = simple.opt font.opt
all: $(PROGRAMS)

ML_SOURCES = simple.ml

REQUIRES = algen graphics geom

include ../make.common

%.cmo: %.ml $(top_srcdir)/oaah.cma
	$(OCAMLC) -package "$(REQUIRES)" -I $(top_srcdir) $(OCAMLFLAGS) -c $<

%.cmx: %.ml $(top_srcdir)/oaah.cmxa
	$(OCAMLOPT) -package "$(REQUIRES)" -I $(top_srcdir) $(OCAMLOPTFLAGS) -c $<

%.opt: %.cmx $(top_srcdir)/oaah.cmxa
	$(OCAMLOPT) -package "$(REQUIRES)" -I $(top_srcdir) oaah.cmxa -linkpkg $(OCAMLOPTFLAGS) $< -o $@

clean-spec:

distclean-spec:

-include .depend
