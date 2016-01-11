# p3r7's .emacs.d

A moderately simple emacs config.

## Installation

As submodules are present, you'd need a recursive clone:

	git clone --recursive https://github.com/p3r7/emacs.d .emacs.d

If you forgot it, you can add them afterwards:

	cd .emacs.d
	git submodule update --init --recursive

If I ever forgot to refference last revision of each submodule, you might want to:

	git submodule foreach --recursive git pull origin master