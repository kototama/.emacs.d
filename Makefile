all: dirs submodules install

dirs:
	if [ ! -d "backups" ]; then mkdir backups; fi
	if [ ! -d "elpa/color-theme-20070910.1007/themes" ]; then mkdir -p elpa/color-theme-20070910.1007/themes/; fi

submodules:
	git submodule init
	git submodule update

# Install MELPA packages
install:
	emacs --batch -l ~/.emacs.d/setup.el

clean:
	rm -rf ~/.emacs.d/elpa
