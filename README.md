# My Personal Emacs Configuration

This is my personal Emacs configuration. This is the second repository
I clone -- after [my dotfiles](https://github.com/skeeto/dotfiles) --
when settling in on a new computer . I'm using `package.el` and Emacs
24 themes, so **this requires at least Emacs 24.3!**

To use it, clone it into your home directory, or at least wherever
Emacs thinks your home directory is. Move your existing `.emacs` file
out of the way, since it being there prevents Emacs from using the
`init.el` in this repository. I do still use a `.emacs` file for
system-specific configuration, but I put this at the top,

```cl
(let ((init "~/.emacs.d/init.elc"))
  (if (file-exists-p init)
      (load-file init)
    (load-file (substring init 0 -1))))
```

If you did everything right Emacs should simply launch with no
errors. You will be greeted with a featureless, empty gray box
awaiting your instructions.

## Features

I try to keep my Emacs configuration tight and tidy. I generally spend
somewhere between 30-60 hours a week in front of Emacs, so this
configuration has been carefully pieced together and every line is
important.

### Whitespace

There's a customized hook for `whitespace-cleanup` in
`before-save-hook`. It will remove all trailing whitespace, and,
because I'm so picky, also convert all tabs to spaces just before
saving a buffer. To disable this on a per-buffer basis -- for example,
to precisely edit a sloppily-spaced file -- run
`toggle-whitespace-cleanup`.

See `whitespace-cleanup.el` for all the details.

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
modes. It can also do live "evaluation" of CSS (`skewer-css-mode`) and
HTML (`skewer-html-mode`).

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
