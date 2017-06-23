EMACS   = emacs
VERSION = $$($(EMACS) -Q -batch --eval '(princ emacs-version)')

compile: init
	$(EMACS) -Q -batch -l init.el -l compile.el

init:
	$(EMACS) -Q -batch -L lisp -l packages.el
	cp -r lisp etc site-lisp/$(VERSION)

clean:
	rm -rf auto-save-list
	rm -rf site-lisp/*/lisp site-lisp/*/etc

mostlyclean:
	rm -rf auto-save-list
	rm -rf site-lisp

distclean:
	rm -rf auto-save-list
	rm -rf site-lisp
	rm -rf gpkg
