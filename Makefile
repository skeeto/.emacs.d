clean:
	find lisp etc -name "*.elc" -exec $(RM) {} \;
	$(RM) init.elc
