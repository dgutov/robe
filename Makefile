EMACS = emacs

.PHONY: ert

ert:
	${EMACS} --batch --eval "(package-initialize)" -l robe.el \
	-l ert/core-tests.el -l ert/completion-tests.el \
	-f ert-run-tests-batch-and-exit

deps:
	wget https://raw.githubusercontent.com/nonsequitur/inf-ruby/master/inf-ruby.el
	${EMACS} --batch --eval "(package-install-file \"inf-ruby.el\")"
