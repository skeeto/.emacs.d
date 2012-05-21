# My Personal Emacs Configuration

What you see before you is my personal Emacs configuration. This is
the very first repository I recursively clone when settling in on a
new computer. Currently it should work with both Emacs 23 and the
bleeding-edge Emacs 24. If you clone this repository non-recursively,
don't forget to recursively clone the submodules too. They're
critical, because this is primarily a repository of repositories.

    git submodule update --init --recursive

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

Particularly special ones are `compile-bind*` and `ant-bind*`
(i.e. Apache Ant specifically). This creates a special compile
keybinding for a specific build system target. I use these bindings
literally hundreds of times per day.

When these bindings are used, the current buffer is automatically
saved and the build system is run with the pre-selected target. Give
it a prefix argument and it uses a numbered compilation
buffer. `compile-bind*` creates keybindings in a given keymap and
`ant-bind*` creates them for `java-mode` only. For example, these two
bindings are already defined globally,

```cl
(compile-bind*
 (current-global-map)
 'make ("C-x c" 'compile
        "C-x C" 'clean))  ; Note the capital C
```

`C-x c` runs `make` with the `compile` target and `C-x C` runs `make`
with the `clean` target.

`ant-bind*` is part of `java-mode-plus`.

### Whitespace

There's a customized hook for `whitespace-cleanup` in
`before-save-hook`. It will remove all trailing whitespace, and,
because I'm so picky, also convert all tabs to spaces just before
saving a buffer. To disable this on a per-file basis -- for example,
to precisely edit a sloppily-spaced file -- set the buffer-local
variable `do-whitespace-cleanup` to `nil`.

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

I use [YASnippet](https://github.com/capitaomorte/yasnippet), a Git
mirror of which is provided for this configuration. It allows you to
type a keyword and hit tab, expanding it into a full code *snippet*,
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

This is one of the big ones. Because I spend so much time writing
Java, and Emacs was lacking solid Java support, I built up my own
extensions to support Java development. The full documentation is
listed in the
[`java-mode-plus.el` header](https://raw.github.com/skeeto/emacs-java/master/java-mode-plus.el),
but here's a quick rundown of my configuration.

First, my extensions do class completion and look-up by examining the
file structure of Javadoc documentation. So to make any effective use
of these extensions you need to tell it where to find some Javadoc
hierarchies. This is another great use of a system-specific `.emacs`
file, since these locations vary from system to system. Here's an
example from the current machine I'm using.

```cl
(java-docs "/usr/share/doc/openjdk-6-jdk/api"
           "~/doc/lwjgl"
           "~/doc/commons-cli"
           "~/doc/guava"
           "~/doc/jline"
           "~/doc/rxtx"
           "~/doc/junit4")
```

I gave it the standard Javadoc path and a bunch more I keep in my home
directory. These directories are scanned and the results cached in
`~/.java-docs/` for faster loading in the future. Once you've done
this you can jump to any class's documentation with `C-h j`. I use
Firefox, so this will try to open the documentation in Firefox if
possible.

If you don't specify the core Java Javadoc, it will attempt add it for
you, linking to the official website rather than a file on the local
system.

The `javadoc` directory in this repository contains a build script
that uses Apache Ivy to fetch my preferred Javadoc jars from the Maven
repository. All you need to do is run `ant` in this directory and the
documentation will be fetched and unzipped. Everything fetched will be
automatically loaded into java-docs by `init.el` on the next Emacs
startup. Edit `ivy.xml` to add more default documentation.

Next, if you're in a `java-mode` buffer, you can add an import to the
imports section at any time with `C-x I` (the
`java-mode-short-keybindings` binding). Like `java-docs-lookup`, `C-h
j`, Ido will do a completing read asking you which class you'd like to
import. It's inserted in the proper position inside the imports
section.

If you're using
[Ant like I do](https://github.com/skeeto/SampleJavaProject), there
are a number of bindings that trigger specific Ant targets. The three
I use most often are,

* `C-x c` -- Compile the program.
* `C-x r` -- Run the program.
* `C-x t` -- Run the unit tests.

The compilation buffer name is derived from the prefix argument. Using
prefixes (`C-u` *number*), you can run any number of compilation
processes at once. I find this especially useful for hot code
replacement.

The YASnippets in `emacs-java/snippets/` take advantage of my
`java-mode` extensions. My favorite snippet is `cls` ("class"): start
a fresh Java source file inside a package, type `cls`, and hit tab. It
will attempt to guess the package name from your path and fill out the
basics with it, allowing you to quickly get on with your business.
