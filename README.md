# My Personal Emacs Configuration

What you see before you is my personal Emacs configuration. This is
the very first repository I clone when settling in on a new
computer. Currently it should work with both Emacs 23 and the
bleeding-edge Emacs 24. If you clone this repository, don't forget to
clone the submodules too. They're critical, because this is primarily
a repository of repositories.

    git submodule update --init

Make sure you clone it into your home directory, or at least wherever
Emacs thinks your home directory is. Move your existing `.emacs` file
out of the way, since it being there prevents Emacs from using the
`init.el` in this repository. I do still use a `.emacs` file for
system-specific configuration, but I put this at the top,

```cl
(load-file "~/.emacs.d/init.el")
```

If you did everything right Emacs should simply launch with no errors
and maybe just one warning from YASnippet. You will be greeted with a
featureless, empty gray box awaiting your instructions.

## Features

I keep my Emacs configuration light. Almost everything is either
written by me or is something I use regularly. I generally spend
somewhere between 35-80 hours a week in front of Emacs, so this
configuration has been carefully pieced together and every line is
important.

### The Little Things

Here's a list of some small features added by `init.el` or
`my-funcs.el`.

* `M-<up>` -- Move the current line up.
* `M-<down>` -- Move the current line down.
* `C-x !` -- Insert a UUID.
* `C-x ~` -- Set the current buffer to 80 columns.
* `C-c r` -- Insert random number.
* `C-c e` -- Eval and replace.
* `M-g` -- Go to line.
* `C-x C-k` -- Compile.
* `[f1]` -- Start a terminal.
* `[f2]` -- Revert the buffer, *no questions asked*.
* `[pause]` -- Dedicate the current window to the current buffer.

And some of the more interesting functions/macros, especially if
you're an Elisp developer,

* `unfill-paragraph`
* `dos2unix` and `unix2dos`
* `expose`
* `measure-time`

Particularly special ones are `make-bind*` (i.e. Makefiles) and
`ant-bind*` (i.e. Apache Ant). This creates a special compile binding
for a particular target. I use these bindings literally hundreds of
times per day.

When used, the current buffer is automatically saved and the build
system is run with the pre-selected target. Give it a prefix argument
and it uses a numbered compilation buffer. `make-bind*` creates global
keybindings and `ant-bind*` creates them for `java-mode` only. For
example, this one was already defined,

```cl
(make-bind* "C-x C" 'clean)   ; Note the capital C
```

Hitting `C-x C` runs `make` with the `clean` target.

`ant-bind*` is part of `java-mode-plus`.

### Ido Mode

Something experienced Emacs users may notice at first is that I make
heavy use of `ido-mode`. It's turned on with flex matching, I've got
`smex` loaded to complete `M-x` commands, and I use it for picking
Java documentation. It's a wonderful feature and every Emacs user
should be using it.

### Magit

To interact with Git repositories, I use
[Magit](http://philjackson.github.com/magit/) (pronounced like
"magic"). You can run it at any time with `C-x g`. As
[the manual](http://philjackson.github.com/magit/magit.html) points
out, Magit is not a complete interface for Git, nor should it be. It
covers 95% of my Git use, with the other 5% directly with Git on the
command line. Magit is one of the main reasons I use Git.

### Web Server

This configuration includes a built-in web server, written by me. You
can launch it with `httpd-start` and stop it with `httpd-stop`. It
will serve files from the directory at `httpd-root` and log messages
in s-expression form to the `*httpd*` buffer.

I don't use this much, but I think it's cool. It also makes a great
demo.

### YASnippet

I use [YASnippet](http://code.google.com/p/yasnippet/), a Git mirror
of which is provided for this configuration. It allows you to type a
keyword and hit tab, expanding it into a full code *snippet*,
sometimes with further assistance filling out components of the
snippet. There are way too many of them to describe here, so look in
`yasnippet/snippets/`, `yasnippet-java/`, and `emacs-java/snippets` to
learn what's there. If you're too lazy to look and just want to see
one work, when in a code-oriented mode type `if` and hit tab.

### Lisp: Paredit and Parenface

[Paredit](http://www.emacswiki.org/emacs/ParEdit) is a very
significant behavioral change for Lisp modes. It enforces parenthesis
balance and provides all sorts of shortcuts for manipulating entire
s-expressions at once. It may feel annoying at first, but it quickly
becomes indispensable. Keep looking at the
[cheatsheet](http://www.emacswiki.org/emacs/PareditCheatsheet) until
you've got the hang of it.

I also have parenface configured, which darkens the parenthesis in
Lisp modes. It makes Lisp code all the more pleasing to look at --
reading Lisp is all about indentation, not parenthesis. It won't be
enabled in the `*scratch*` buffer automatically because that buffer
gets set up before parenface does.

You may wonder why I don't have
[Slime](http://common-lisp.net/project/slime/) here. That's because I
let [Quicklisp](http://www.quicklisp.org/) install and manage it for
me. Remember my note about using `.emacs` for system-specific
configuration? That's where I put the hook to load Slime.

### Java

TODO: write this section

See the
[`java-mode-plus.el` header](/skeeto/emacs-java/master/java-mode-plus.el)
for full documentation.
