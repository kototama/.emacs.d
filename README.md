[![Build Status](https://www.travis-ci.org/kototama/.emacs.d.svg?branch=master)](https://www.travis-ci.org/kototama/.emacs.d)

# Emacs configuration

A super lean Emacs configuration with [MELPA](http://melpa.milkbox.net/) and [use-package](https://github.com/jwiegley/use-package).


## Usage

    git clone git@github.com:kototama/.emacs.d.git
    cd .emacs.d
    make

This will install git submodules, Emacs packages and byte-compiles
Emacs Lisp files.

## Packages

Common packages are defined in ```~/.emacs.d/lisp/packages.el```.

User/machine specific packages can be defined with the
```add-packages``` function in:

    $HOME/.eroles/login@hostname/setup.el

## Configuration

User/machine specific configuration can be defined in:

    $HOME/.eroles/login@hostname/init.el

Configurations are stored in the ```~/.emacs.d/lisp/config.el``` with org-like sections accessible with ```orgstruct-mode```.

## Keybindings

The table of user-defined keybindings can be seen with ```M-x
describe-personal-keybindings```.
