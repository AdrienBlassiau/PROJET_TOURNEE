OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
PDFLATEX=pdflatex

INCLUDES=graphics.cma unix.cma
OCAMLCFLAGS=$(INCLUDES)
OCAMLOPTFLAGS=$(INCLUDES)
PDFLATEXFLAGS=-halt-on-error

################################################################################

# Phase 1
PHASE1_INTERFACES=useful.mli my_stack.mli graph.mli ville.mli read.mli city_set.mli draw.mli kd_tree.mli convex_hull.mli tournee.mli main_graph_non_complet.mli
PHASE1_SOURCES=useful.ml my_stack.ml graph.ml ville.ml read.ml city_set.ml draw.ml kd_tree.ml convex_hull.ml tournee.ml main_graph_non_complet.ml

non_complet: $(PHASE1_INTERFACES) $(PHASE1_SOURCES:.ml=.cmo)
	$(OCAMLC) -o run $(OCAMLCFLAGS) $(PHASE1_SOURCES:.ml=.cmo)

###############################################################################

# Phase 2
PHASE2_INTERFACES=useful.mli my_stack.mli graph.mli ville.mli read.mli city_set.mli draw.mli kd_tree.mli convex_hull.mli tournee.mli main_graph_complet.mli
PHASE2_SOURCES=useful.ml my_stack.ml graph.ml ville.ml read.ml city_set.ml draw.ml kd_tree.ml convex_hull.ml tournee.ml main_graph_complet.ml

complet: $(PHASE2_INTERFACES) $(PHASE2_SOURCES:.ml=.cmo)
	$(OCAMLC) -o run $(OCAMLCFLAGS) $(PHASE2_SOURCES:.ml=.cmo)


###############################################################################

# Test unitaires
PHASE3_INTERFACES=graph.mli useful.mli my_stack.mli convex_hull.mli city_set.mli kd_tree.mli read.mli draw.mli ville.mli tournee.mli main_graph_complet.mli main_graph_non_complet.mli test.mli
PHASE3_SOURCES=graph.ml useful.ml my_stack.ml convex_hull.ml city_set.ml kd_tree.ml read.ml draw.ml ville.ml tournee.ml main_graph_complet.ml main_graph_non_complet.ml test.ml \
test_graph.ml test_useful.ml test_my_stack.ml test_convex_hull.ml test_city_set.ml test_kd_tree.ml test_read.ml test_ville.ml main_test.ml

test_unitaires: $(PHASE3_INTERFACES) $(PHASE3_SOURCES:.ml=.cmo)
	$(OCAMLC) -o run $(OCAMLCFLAGS) $(PHASE3_SOURCES:.ml=.cmo)


###############################################################################

# Test tournée complet
PHASE4_INTERFACES=graph.mli useful.mli my_stack.mli convex_hull.mli city_set.mli kd_tree.mli read.mli draw.mli ville.mli tournee.mli main_graph_complet.mli
PHASE4_SOURCES=graph.ml useful.ml my_stack.ml convex_hull.ml city_set.ml kd_tree.ml read.ml draw.ml ville.ml tournee.ml main_graph_complet.ml test_tournee_complet.ml

test_complet: $(PHASE4_INTERFACES) $(PHASE4_SOURCES:.ml=.cmo)
	$(OCAMLC) -o run $(OCAMLCFLAGS) $(PHASE4_SOURCES:.ml=.cmo)

###############################################################################

# Test tournée non complet
PHASE5_INTERFACES=graph.mli useful.mli my_stack.mli convex_hull.mli city_set.mli kd_tree.mli read.mli draw.mli ville.mli tournee.mli main_graph_non_complet.mli
PHASE5_SOURCES=graph.ml useful.ml my_stack.ml convex_hull.ml city_set.ml kd_tree.ml read.ml draw.ml ville.ml tournee.ml main_graph_non_complet.ml test_tournee_non_complet.ml

test_non_complet: $(PHASE5_INTERFACES) $(PHASE5_SOURCES:.ml=.cmo)
	$(OCAMLC) -o run $(OCAMLCFLAGS) $(PHASE5_SOURCES:.ml=.cmo)


###############################################################################

# Rapport
RAPPORT_TEX=rapport/rapport.tex
RAPPORT_DIR=rapport/

rapport.pdf:
	$(PDFLATEX) $(PDFLATEXFLAGS) -output-directory $(RAPPORT_DIR) $(RAPPORT_TEX)
rapport: rapport.pdf

# Compilation des éléments importants
all: rapport.pdf normal

################################################################################

# Règles de compilation exécutées à chaque make
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLCFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLCFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<

.depend:
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

################################################################################

# Nettoyage des fichiers temporaires
clean:
	rm -f *.cm[iox]
	rm -f *.o
	rm -f run
	rm -f rapport/*{.aux,.log,.out,.toc}

include .depend
