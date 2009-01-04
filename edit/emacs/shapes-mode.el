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
;; to such. That said, it does support compilation-mode, comment-dwim, Imenu,
;; skeleton-pairs, and viewing output through doc-view.
;;
;; TODO
;; - Syntax highlighting (first: comment and string faces)
;; - motion commands
;; - automatic indentation
;; - Hide compilation buffer if a doc-view buffer is visible and there are no
;;errors when recompiling.
;; - PDF sync between source and output (path control points etc).
;; - Automatically pretty-print #, -> (use abbrevs!)
;; - Convert Shapes ref manual to info and hook up to Info-Look
;; - Skeletons?
;; - Eldoc mode?
;; - Some sort of camelCase support a la subword mode of cc-mode
;; - Investigate Flymake support

;; BUGS
;; - Investigate mismatch between Shapes and Emacs column numbers.
;;   Note: Emacs column 0 means that *point* is before the first char. Column 1
;; * point is after first char (and the cursor is ON char 2).
;; - Handle the case when shapes-mode is started in a buffer that isn't saved.
;; - Handle the case when shapes-mode is activated on a buffer that doesn't end
;; with .sh(ape|ext), so that its encoding is not set to utf-8 correctly, that
;; is change the buffer encoding to utf-8 if it is something else.

;;; Installation:

;; To install, put this file somewhere in your load path and just require
;; 'shapes-mode from your ~/.emacs or equivalent, like so:
;;
;; (require 'shapes-mode)

;; Code tested only on GNU Emacs 22.

;;; Code:

(defcustom shapes-compile-command "shapes"
  "Name of the Shapes compiler executable, and any options to pass to it."
  :type 'string)

(defcustom shapes-unicode-pretty-print t
  "Whether to pretty-print Shapes idenifiers using their Unicode
equivalents or not."
  :type 'boolean)

(defconst shapes-basic-indent-width 2)

(defconst shapes-file-regex "\\.sh\\(ape\\|ext\\)$"
  "Regular expression matching Shapes source files.")

(add-to-list 'auto-mode-alist (cons shapes-file-regex 'shapes-mode))

;; The Shapes compiler wants its input utf-8, LF line-endings (for now).
(modify-coding-system-alist 'file shapes-file-regex 'utf-8-unix)

(defvar shapes-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; (modify-syntax-entry ?\" ".   " st)
;;     (modify-syntax-entry ?\\ ".   " st)
;;     ;; Add `p' so M-c on `hello' leads to `Hello', not `hello'.
;;     (modify-syntax-entry ?' "w p" st)
    st)
  "Syntax table used while in `shapes-mode'.")

(defvar shapes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-c" 'shapes-compile)
    (when (featurep 'doc-view)		; Emacs 22 and lower does not ship with
					; doc-view
      (define-key map "\C-c\C-v" 'shapes-view))
    (mapc (lambda (elt) 
	    (define-key map (string elt) 'skeleton-pair-insert-maybe))
	  "`([{")
    ;; (define-key map "\C-cl" 'shapes-lambda)
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

;; (defun shapes-beginning-of-defun ()
;;   "Move backward to the beginning of a defun.
;; Every top level binding of a function is considered to be a defun."
;;   (interactive)
;;   ;; match identifier: \ arg1 .. argn -> body-expr
;;   (re-search-backward "^")
;;  )

(defun shapes-indent-line ()
  "Function that handles indentation of Shapes programs.

The syntax tables will probably require some tweaking."
  (let ((pos (- (point-max) (point)))	; We store pos relative to end of file,
					; so that we can go back to it even
					; after indentation has inserted (before
					; point).
	(beg (line-beginning-position)))
    ;; We really want to indent w.r.t to the first thing on the line.
    (goto-char beg)			
    (let* ((state (syntax-ppss (point)))
	   (open-pos (elt state 1)))
      (delete-horizontal-space)
      ;; Find column number of first expression after open-pos, indent to it.
      (indent-to
       (save-excursion
	 (save-restriction
	   (goto-char open-pos)
	   (let ((open-col (current-column)))
	     ;; We only want to scan this line.
	     (narrow-to-region (point) (line-end-position))
	     (forward-char)
	     (skip-chars-forward " \t")
	     ;; [           foo   bar
	     ;; ^: open-pos ^: s1 ^: s2
	     (let ((s1-col (current-column)))
	       (forward-sexp)
	       (if (= (current-column) s1-col) ; Did we not move over something?
		   (+ open-col shapes-basic-indent-width)
		 (progn
		   (skip-chars-forward " \t")
		   (let ((s2-col (current-column)))
		     (forward-sexp)
		     (if (= (current-column) s2-col)
			 ;; No more expression.
			 (+ s1-col shapes-basic-indent-width)
		       ;; There was another expression, align with it.
		       s2-col)))))))))
      (goto-char (- (point-max) pos)))))


(defun shapes-mode ()
  "Major mode for editing Shapes programs.
\\{shapes-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'shapes-mode)
  (setq mode-name "Shapes")
  (use-local-map shapes-mode-map)
  (set-syntax-table shapes-mode-syntax-table)

  ;; Skeletons
  (set (make-local-variable 'skeleton-pair-alist)
       '((?` _ ?´)			; for strings
	 (?{ \n _ \n ?})))			

  ;; (define-skeleton shapes-lambda "Function template skeleton."
  ;;     "Formal parameters: "
  ;;     "\\ " str (if shapes-unicode-pretty-print " → " " -> ") _)

  (when shapes-unicode-pretty-print
    (setq abbrev-mode t))
  
  (setq comment-start-skip "/\\*\\*+ *\\||\\*\\*+ *")
  (setq comment-start "|**")

  (setq indent-line-function (function shapes-indent-line)) 

  ;; Compilation-mode support.
  ;; Example data:
  ;;  /Users/tger/stroke.shape:1(8-10): The unit b is unbound
  (eval-after-load 'compile
    '(progn
       (add-to-list 'compilation-error-regexp-alist
		    '("\\(.*\\):\\([0-9]+\\)(\\([0-9]+\\)-\\([0-9]+\\)):"
		      1 2 (3 . 4) nil 1))
       ;; The Shapes compiler works this way
       (setq compilation-error-screen-columns nil)))

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

;; Local Variables: **
;; coding:utf-8! **
;; End: **