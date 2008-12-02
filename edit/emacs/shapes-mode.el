;;; shapes-mode.el --- Emacs major mode for editing Shapes programs

;; Copyright (C) 2008 Tobias Gerdin

;; This file is part of Shapes.

;; Shapes is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; any later version.

;; Shapes is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Shapes.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This mode is very much work in progress. It does not handle automatic
;; indentation of Shapes programs nor does it offer any motion commands adapted
;; to such. That said, it does support compilation-mode, comment-dwim, Imenu and
;; viewing output through doc-view.
;;
;; TODO
;; - Syntax highlighting (first: comment and string faces)
;; - motion commands
;; - automatic indentation
;; - Hide compilation buffer if a doc-view buffer is visible and there are no
;;errors when recompiling.
;; - PDF sync between source and output (path control points etc).
;; - Automatically pretty-print #, \, ->

;; BUGS
;; - Investigate mismatch between Shapes and Emacs column numbers.
;;   Note: Emacs column 0 means that *point* is before the first char. Column 1
;; * point is after first char (and the cursor is ON char 2).

;;; Installation

;; To install, put this file somewhere in your load path and just require
;; 'shapes-mode from your ~/.emacs or equivalent, like so:
;;
;; (require 'shapes-mode)

;; Code tested only on GNU Emacs 22.

(defcustom shapes-compile-command "~/shapes/source/shapes"
  "Name of the Shapes compiler executable, and any options to pass to it."
  :type 'string)

(defconst shapes-file-regex "\\.sh\\(ape\\|ext\\)$"
  "Regular expression matching Shapes source files.")

(add-to-list 'auto-mode-alist (cons shapes-file-regex 'shapes-mode))

;; The Shapes compiler wants its input utf-8, LF line-endings (for now).
(modify-coding-system-alist 'file shapes-file-regex 'utf-8-unix)
	     
;; Example data:
;;  /Users/tger/stroke.shape:1(8-10): The unit b is unbound
(eval-after-load 'compile
  '(add-to-list 'compilation-error-regexp-alist
		'("\\(.*\\):\\([0-9]+\\)(\\([0-9]+\\)-\\([0-9]+\\)):"
		  1 2 (3 . 4) nil 1)))

;;; TBD
;; (defvar shapes-mode-syntax-table
;;   (let ((st (make-syntax-table)))
;;     (modify-syntax-entry ?\" ".   " st)
;;     (modify-syntax-entry ?\\ ".   " st)
;;     ;; Add `p' so M-c on `hello' leads to `Hello', not `hello'.
;;     (modify-syntax-entry ?' "w p" st)
;;     st)
;;   "Syntax table used while in `text-mode'.")

(defvar shapes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'shapes-compile)
    (when (featurep 'doc-view)		; Emacs 22 and lower does not ship with
					; doc-view
      (define-key map "\C-c\C-v" 'shapes-view))
    map))

(defun shapes-compile ()
  "Compiles the source file."
  (interactive)
  (compile (concat shapes-compile-command " " (buffer-file-name))))

(defun shapes-view ()
  "Compiles the source file and, if compilation is successful, views it using
doc-view."
  (interactive)
  ;; Here we basically pass the code to view the output as a continuation to the
  ;; compile function since compilation is performed asynchronously in Emacs.
  (add-to-list
   'compilation-finish-functions
   (lambda (comp-buf exit-msg)
     ;; I am confused as to which buffer is really active when we are
     ;; applied. (buffer-name) reports *compilation* but (window-buffer) reports
     ;; the Shapes program buffer.
     (setq compilation-finish-functions (cdr compilation-finish-functions))
     (when (equal exit-msg "finished\n")
       (let ((output (concat (file-name-sans-extension
			      (buffer-file-name (window-buffer)))
			     ".pdf")))
	 (with-selected-window
	     (next-window)		; This way image will hopefully reuse
					; the compilation buffer.
	   (doc-view t output))))))
  (shapes-compile))

(defun shapes-mode ()
  "Major mode for editing Shapes programs.
\\{shapes-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'shapes-mode)
  (setq mode-name "Shapes")
  (use-local-map shapes-mode-map)
  ;(set-syntax-table shapes-mode-syntax-table)

  (setq comment-start-skip "/\\*\\*+ *\\||\\*\\*+ *")
  (setq comment-start "|**")

  ;; The Shapes compiler tabs philosophy is different from that of Emacs, so for
  ;; now we do this.
  (setq indent-tabs-mode nil)

  ;; Simplified recognition of a top-level Shapes identifier. Probably needs
  ;; more work.
  (setq imenu-generic-expression '((nil "^\\([a-zA-Z0-9_?]+\\):" 1)
				   (nil "^dynamic\\s-+\\(@[a-zA-Z0-9_?]+\\)" 1)))

  (unless (or (file-exists-p "makefile")
	      (file-exists-p "Makefile"))
    (set (make-local-variable 'compile-command)
	 (concat shapes-compile-command " "
		 (file-name-sans-extension buffer-file-name)))))

(provide 'shapes-mode)
