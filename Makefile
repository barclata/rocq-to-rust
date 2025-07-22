# Coq project makefile

# Documentation target.  Type "make DOC=all-gal.pdf" to make PDF.
DOC	?= gallinahtml

# File $(PROJ) contains the list of source files.
# See "man coq_makefile" for its format.
PROJ	= _CoqProject

# Generated makefile
COQMK	= coq.mk

TEMPLATE_REPO = https://github.com/Durbatuluk1701/coq-templates.git

COQBIN?=
ifneq (,$(COQBIN))
# add an ending /
COQBIN:=$(COQBIN)/
endif

all:	
	dune build

test: all
	dune test

clean:
	dune clean

# Generates the meta files for the project
meta: 
	TMP=`mktemp -d` && \
	git clone $(TEMPLATE_REPO) $$TMP && \
	$$TMP/generate.sh

.PHONY:	all meta
