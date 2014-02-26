all: dirs submodules install compile

dirs:
	if [ ! -d "backups" ]; then mkdir backups; fi

submodules:
	git submodule init
	git submodule update

compile:
	emacs -batch -l ~/.emacs.d/init.el -f batch-byte-compile ./init.el ./lisp/*.el `find ~/.eroles -name \*.el`

# Install MELPA packages
install:
	emacs --batch -l ~/.emacs.d/setup.el

clean:
	rm -rf ~/.emacs.d/elpa
