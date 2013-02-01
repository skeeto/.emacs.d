# My Personal Emacs Configuration

What you see before you is my personal Emacs configuration. This is
the very first repository I recursively clone when settling in on a
new computer. I'm using `package.el` and Emacs 24 themes, so **this
requires Emacs 24!** If you clone this repository non-recursively,
don't forget to recursively clone the submodules too. They're
critical.

    git submodule update --init --recursive

Make sure you clone it into your home directory, or at least wherever
Emacs thinks your home directory is. Move your existing `.emacs` file
out of the way, since it being there prevents Emacs from using the
`init.el` in this repository. I do still use a `.emacs` file for
system-specific configuration, but I put this at the top,

```cl
(load-file "~/.emacs.d/init.el")
```

If you did everything right Emacs should simply launch with no
errors. You will be greeted with a featureless, empty gray box
awaiting your instructions.

## Features

I keep my Emacs configuration light. Almost everything is either
written by me or is something I use regularly. I generally spend
somewhere between 35-80 hours a week in front of Emacs, so this
configuration has been carefully pieced together and every line is
important.

### The Little Things

Here's a list of some small features added by `init.el` or
`my-funcs.el`.

* <kbd>M-<up></kbd> -- Move the current line up.
* <kbd>M-<down></kbd> -- Move the current line down.
* <kbd>C-x !</kbd> -- Insert a UUID.
* <kbd>C-x ~</kbd> -- Set the current buffer to 80 columns.
* <kbd>C-c r</kbd> -- Insert random number.
* <kbd>C-c e</kbd> -- Eval and replace.
* <kbd>M-g</kbd> -- Go to line.
* <kbd>C-x C-k</kbd> -- Compile.
* <kbd>[f1]</kbd> -- Start a terminal.
* <kbd>[f2]</kbd> -- Revert the buffer, *no questions asked*.
* <kbd>[pause]</kbd> -- Dedicate the current window to the current buffer.

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

<kbd>C-x c</kbd> runs `make` with the `compile` target and <kbd>C-x
C</kbd> runs `make` with the `clean` target.

`ant-bind*` is part of `ant-project-mode`.

### Whitespace

There's a customized hook for `whitespace-cleanup` in
`before-save-hook`. It will remove all trailing whitespace, and,
because I'm so picky, also convert all tabs to spaces just before
saving a buffer. To disable this on a per-file basis -- for example,
to precisely edit a sloppily-spaced file -- set the buffer-local
variable `do-whitespace-cleanup` to `nil`.

### Ido

Something experienced Emacs users may notice at first is that I make
heavy use of `ido-mode`. It's turned on with flex matching, I've got
`smex` loaded to complete <kbd>M-x</kbd> commands, and I use it for
picking Java documentation. It's a wonderful feature and every Emacs
user should be using it.

### Magit

To interact with Git repositories, I use
[Magit](http://philjackson.github.com/magit/) (pronounced like
"magic"). You can run it at any time with <kbd>C-x g</kbd>. As
[the manual](http://philjackson.github.com/magit/magit.html) points
out, Magit is not a complete interface for Git, nor should it be. It
covers 95% of my Git use, with the other 5% directly with Git on the
command line. Magit is one of the main reasons I use Git.

### Web Server

This configuration includes a built-in web server, written by me. You
can launch it with `httpd-start` and stop it with `httpd-stop`. It
will serve files from the directory at `httpd-root` and log messages
in s-expression form to the `*httpd*` buffer.

#### Skewer

The most important use of the web server is for
[skewer-mode](https://github.com/skeeto/skewer-mode). I run its
associated userscript in my browser allowing me to, at *any* time on
*any* page, attach Emacs to a webpage. Once attached to Emacs I can
fully interact with the browser's JavaScript runtime through a
JavaScript REPL (`skewer-repl`) and live expression evaluation in
JavaScript buffers, very similar to the various lisp interaction
modes. It can also do live "evaluation" of CSS (`skewer-css-mode`).

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

#### nrepl.el

I interact with Clojure projects using
[`nrepl.el`](https://github.com/kingtim/nrepl.el) and nREPL. This can
be fired up at any time with `nrepl-jack-in`. This requires Leiningen,
[which is very easy to set up](http://nullprogram.com/blog/2013/01/07/).

### Java

This was one of the big ones when I used spend a lot of time writing
Java, and Emacs was lacking solid Java support. I built up my own
extensions to support Java development: `ant-project-mode` and
`javadoc-lookup`. The full documentation is listed in the
[`ant-project-mode.el` header](https://github.com/skeeto/ant-project-mode/blob/master/ant-project-mode.el),
but here's a quick rundown of my configuration.

First, I use
[javadoc-lookup](https://github.com/skeeto/javadoc-lookup) for class
completion and lookup, which works by examining the file structure of
Javadoc documentation. While it comes with the core API Javadoc
built-in, to make more effective use of it I need to tell it about
what other things I'll want to look up. Its configuration looks like
this,

```cl
(javadoc-add-artifacts [org.lwjgl.lwjgl lwjgl "2.8.2"]
                       [com.nullprogram native-guide "0.2"]
                       [joda-time joda-time "2.1"])
```

I can jump to any class's documentation with <kbd>C-h j</kbd>. I use
Firefox, so this will try to open the documentation in Firefox if
possible.

> Notice: *`ant-project-mode` is not currently part of this
> configuration, so the following will not work at the moment.*

Next, when in a `java-mode` buffer, I can add an import to the imports
section at any time with <kbd>C-x I</kbd> (the
`ant-project-short-keybindings` binding). Like `javadoc-lookup`,
<kbd>C-h j</kbd>, Ido will do a completing read asking you which class
I'd like to import. It's inserted in the proper position inside the
imports section.

When using
[Ant like I do](https://github.com/skeeto/SampleJavaProject), there
are a number of bindings that trigger specific Ant targets. The three
I use most often are,

* <kbd>C-x c</kbd> -- Compile the program.
* <kbd>C-x r</kbd> -- Run the program.
* <kbd>C-x t</kbd> -- Run the unit tests.

The compilation buffer name is derived from the prefix argument. Using
prefixes (<kbd>C-u</kbd> *number*), you can run any number of
compilation processes at once. I find this especially useful for hot
code replacement.

The YASnippets in `emacs-java/snippets/` take advantage of my
`java-mode` extensions. My favorite snippet is `cls` ("class"): start
a fresh Java source file inside a package, type `cls`, and hit tab. It
will attempt to guess the package name from your path and fill out the
basics with it, allowing you to quickly get on with your business.
