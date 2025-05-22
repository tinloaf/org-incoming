EMACS = emacs
INSTALL_INFO = install-info
ORG_DIR1 = -L $(word 1,$(wildcard $(HOME)/.emacs.d/elpa/org-9*))
ifeq ($(words $(ORG_DIR1)),1)
	ORG_DIR1 =
endif
ORG_LIB = -l org -l ox-texinfo
ORG_PUB = --eval='(progn (find-file (expand-file-name "org-incoming.org")) (org-texinfo-export-to-info))'
EMACS_ARGS = --batch $(ORG_DIR1) $(ORG_LIB) $(ORG_PUB)

all: info clean
	@echo all done!

# Run this to update the .info file and dir
info: README.org
	cp README.org org-incoming.org
	$(EMACS) $(EMACS_ARGS)
	$(INSTALL_INFO) org-incoming.info dir

clean:
	rm -f org-incoming.org org-incoming.texi

distclean: clean
	rm -f *~ dir org-incoming.info
