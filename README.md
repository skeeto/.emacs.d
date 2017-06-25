# My Personal Emacs Configuration

This is my personal Emacs configuration. This is the second repository
I clone — after [my dotfiles][dot] — when settling in on a new system.
This configuration uses [**Evil-style bindings**][vim] (i.e. Vim
bindings), and **requires at least Emacs 24.4.**

To install all third-party packages, run `make` in this repository
while connected to the internet:

    make

This will clone additional repositories containing the configured
packages (see `packages.el`) into `gpkg/`, install each package under
`site-lisp/<emacs-version>/`, add each to the `load-path`, and
compile. To install/build packages for another version of Emacs, set
the `EMACS` variable:

    make EMACS=emacs26

Package installations for multiple versions of Emacs can safely
coexist side-by-side. If you did everything right Emacs should simply
launch with no errors. You will be greeted with a featureless, empty
gray box awaiting your instructions.


[dot]: https://github.com/skeeto/dotfiles
[vim]: http://nullprogram.com/blog/2017/04/01/
