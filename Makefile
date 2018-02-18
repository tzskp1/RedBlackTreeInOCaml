all : test

test : test.ml redblacktree.cmx

# rules
OL := ocamllex
OY := ocamlyacc
OO := ocamlfind opt
PACKAGES := ppx_deriving.show str
OOC := $(OO) $(addprefix -package ,$(PACKAGES)) -linkpkg -c
OOO := $(OO) $(addprefix -package ,$(PACKAGES)) -linkpkg -linkall -o

.PHONY : clean all

%.cmx : %.ml
	$(OOC) $< $(filter %.cmx,$^)

%.cmi : %.mli
	$(OOC) $< 

% : %.ml
	$(OOC) $< 
	$(OOO) $@ $(filter %.cmx,$^) $@.cmx 

clean : 
	$(RM) *.o
	$(RM) *.cmx
	$(RM) *.cmi
