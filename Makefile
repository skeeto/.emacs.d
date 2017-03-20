clean:
	find lisp etc -name "*.elc" -exec rm {} \;
	rm -f init.elc
	rm -f -r auto-save-list/
