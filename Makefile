all: init install compile

init:
	git submodule init
	git submodule update

compile:
	emacs -batch -l ~/.emacs.d/init.el -f batch-byte-compile ./init.el ./lisp/*.el

# Install MELPA packages
install:
	emacs --batch -l ~/.emacs.d/setup.el

clean:
	rm -rf ~/.emacs.d/elpa
