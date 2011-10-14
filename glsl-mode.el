;;; glsl-mode.el --- major mode for Open GLSL shader files

;; Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.
;;
;; Author: Xavier.Decoret@imag.fr
;; Keywords: languages
;; Version: 1.0
;; X-URL: http://artis.inrialpes.fr/~Xavier.Decoret/resources/glsl-mode/

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Major mode for editing OpenGLSL grammar files, usually files ending
;; with `.vert' and `.frag'.  Is is based on c-mode plus some features
;; and pre-specified fontifications.

;; This package provides the following features:
;;  * Syntax coloring (via font-lock) for grammar symbols and
;;    builtin functions and variables 
;;  * Indentation for the current line (TAB) and selected region (C-M-\).
;;  * Switching between file.vert and file.frag
;;    with S-lefttab (via ff-find-other-file)

;;; Installation:

;; This file requires Emacs-20.3 or higher and package cc-mode.

;; If glsl-mode is not part of your distribution, put this file into your
;; load-path and the following into your ~/.emacs:
;;   (autoload 'glsl-mode "glsl-mode" nil t)
;;   (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
;;   (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))

;; To customize, use `M-x customize-group RET antlr RET' or the custom browser
;; (Emacs->Programming->Languages->Antlr).

;;; Code:

(provide 'glsl-mode)

(eval-when-compile			; required and optional libraries
  (require 'cc-mode)
  (require 'find-file))

(defconst glsls-version "1.0"
  "OpenGLSL major mode version number.")

(defvar glsl-mode-hook nil)

(defvar glsl-mode-map
  (let ((glsl-mode-map (make-sparse-keymap)))
    (define-key glsl-mode-map [S-iso-lefttab] 'ff-find-other-file)    
    glsl-mode-map)
  "Keymap for GLSL major mode")

(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))

(defconst glsl-font-lock-keywords-1
  (list
   '("\\<\\(b\\(?:ool\\|vec[1-4]\\)\\|float\\|i\\(?:nt\\|vec[1-4]\\)\\|mat[234]\\|sampler\\(?:1D\\(?:Shadow\\)?\\|2D\\(?:Shadow\\)?\\|3D\\|Cube\\)\\|v\\(?:ec[1-4]\\|oid\\)\\)\\>" . font-lock-type-face)
   '("\\<\\(attribute\\|break\\|con\\(?:st\\|tinue\\)\\|d\\(?:iscard\\|o\\)\\|else\\|for\\|i\\(?:nout\\|[fn]\\)\\|out\\|return\\|struct\\|uniform\\|varying\\|while\\)\\>" . font-lock-keyword-face)
   '("\\<\\(a\\(?:bs\\|cos\\|ll\\|ny\\|\\(?:si\\|ta\\)n\\)\\|c\\(?:eil\\|lamp\\|\\(?:o\\|ros\\)s\\)\\|d\\(?:Fd[xy]\\|istance\\|ot\\)\\|e\\(?:qual\\|xp2?\\)\\|f\\(?:aceforward\\|loor\\|ract\\|transform\\|width\\)\\|greaterThan\\(?:Equal\\)?\\|inversesqrt\\|l\\(?:e\\(?:ngth\\|ssThan\\(?:Equal\\)?\\)\\|og2?\\)\\|m\\(?:a\\(?:trixCompMult\\|x\\)\\|in\\|ode\\)\\|no\\(?:ise[1-4]\\|rmalize\\|t\\(?:Equal\\)?\\)\\|pow\\|r\\(?:adians\\|ef\\(?:\\(?:le\\|ra\\)ct\\)\\)\\|s\\(?:hadow\\(?:1D\\(?:Lod\\|Proj\\(?:Lod\\)?\\)?\\|2D\\(?:Lod\\|Proj\\(?:Lod\\)?\\)?\\)\\|i\\(?:g?n\\)\\|moothstep\\|qrt\\|tep\\)\\|t\\(?:an\\|exture\\(?:1D\\(?:Lod\\|Proj\\(?:Lod\\)?\\)?\\|2D\\(?:Lod\\|Proj\\(?:Lod\\)?\\)?\\|3D\\(?:Lod\\|Proj\\(?:Lod\\)?\\)?\\|Cube\\(?:Lod\\)?\\)\\)\\)\\>" . font-lock-builtin-face)
   '("\\<\\(gl_\\(?:Back\\(?:Color\\|Light\\(?:\\(?:Model\\)?Product\\)\\|Material\\|SecondaryColor\\)\\|C\\(?:lipVertex\\|olor\\(?:\\)?\\)\\|DepthRange\\(?:\\)?\\|EyePlane[Q-T]\\|F\\(?:og\\(?:Coord\\)?\\|r\\(?:ag\\(?:Co\\(?:lor\\|ord\\)\\|D\\(?:ata\\|epth\\)\\)\\|ont\\(?:Color\\|Facing\\|Light\\(?:\\(?:Model\\)?Product\\)\\|Material\\|SecondaryColor\\)\\)\\)\\|LightSource\\|M\\(?:ax\\(?:\\(?:C\\(?:lipPlane\\|ombinedTextureImageUnit\\)\\|DrawBuffer\\|FragmentUniformComponent\\|Light\\|Texture\\(?:Coord\\|\\(?:Image\\)?Unit\\)\\|V\\(?:aryingFloat\\|ertex\\(?:Attrib\\|\\(?:TextureImageUni\\|UniformComponen\\)t\\)\\)\\)s\\)\\|odelView\\(?:Matrix\\(?:\\(?:Inver\\(?:seTranspo\\)?\\|Transpo\\)se\\)?\\|ProjectionMatrix\\(?:\\(?:Inver\\(?:seTranspo\\)?\\|Transpo\\)se\\)?\\)\\|ultiTexCoord[0-7]\\)\\|Normal\\(?:Matrix\\|Scale\\)?\\|ObjectPlane[Q-T]\\|P\\(?:o\\(?:int\\(?:Size\\)??\\|sition\\)\\|rojectionMatrix\\(?:\\(?:Inver\\(?:seTranspo\\)?\\|Transpo\\)se\\)?\\)\\|SecondaryColor\\(?:\\)?\\|Tex\\(?:Coord\\|ture\\(?:EnvColor\\|Matrix\\(?:\\(?:Inver\\(?:seTranspo\\)?\\|Transpo\\)se\\)?\\)\\)\\|Vertex\\)\\)\\>" . font-lock-variable-name-face)
   )
  "Minimal highlighting expressions for GLSL mode")

(defvar glsl-font-lock-keywords glsl-font-lock-keywords-1
  "Default highlighting expressions for GLSL mode")

(defvar glsl-mode-syntax-table
  (let ((glsl-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" glsl-mode-syntax-table)
    (modify-syntax-entry ?* ". 23" glsl-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" glsl-mode-syntax-table)
    glsl-mode-syntax-table)
  "Syntax table for glsl-mode")

(defvar glsl-other-file-alist
  '(("\\.frag$"
     (".vert"))
    ("\\.vert$"
     (".frag")))
  "Alist of extensions to find given the current file's extension")

(define-derived-mode glsl-mode c-mode "GLSL"
  "Major mode for editing OpenGLSL shader files."
  (set (make-local-variable 'font-lock-defaults) '(glsl-font-lock-keywords))
  (set (make-local-variable 'ff-other-file-alist) 'glsl-other-file-alist)
  )

;;; glsls-mode.el ends here
