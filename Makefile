all : test

redblacktree.cmx: redblacktree.ml redblacktree.cmi
test: test.ml redblacktree.cmx

# rules
OO := ocamlfind opt
PACKAGES := 
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
