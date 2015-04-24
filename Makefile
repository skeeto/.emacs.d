clean:
	find lisp etc -name "*.elc" -exec $(RM) {} \;
	$(RM) init.elc
	$(RM) -r auto-save-list/
