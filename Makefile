.PHONY: all
all: byte opt

byte: oaah.cma
opt: oaah.cmxa

NAME = oaah

ML_SOURCES = oaah.ml oaah_color.ml oaah_image.ml

REQUIRES = algen graphics

include make.common

.PHONY: all opt install uninstall reinstall

%.cmo: %.ml
	$(OCAMLC) -package "$(REQUIRES)" $(OCAMLFLAGS) -c $<

%.cmx: %.ml
	$(OCAMLOPT) -package "$(REQUIRES)" $(OCAMLOPTFLAGS) -c $<


$(NAME).cma: $(ML_OBJS)
	$(OCAMLC)   -a -o $@ -package "$(REQUIRES)" $(OCAMLFLAGS) $(ML_OBJS)

$(NAME).cmxa: $(ML_XOBJS)
	$(OCAMLOPT) -a -o $@ -package "$(REQUIRES)" $(OCAMLOPTFLAGS) $(ML_XOBJS)

install: all
	if test -f $(NAME).cmxa ; then extra="$(NAME).cmxa *.cmx $(NAME).a" ; fi ; \
	ocamlfind install $(NAME) *.cmi $(NAME).cma META $$extra

uninstall:
	ocamlfind remove $(NAME)

reinstall: uninstall install

check: $(NAME).cma $(NAME).cmxa
	$(MAKE) -C tests
	@for t in tests/*.opt ; do $$t ; done
	@echo Ok

clean-spec:
	$(MAKE) -C tests clean

distclean-spec:
	$(MAKE) -C tests distclean

-include .depend
