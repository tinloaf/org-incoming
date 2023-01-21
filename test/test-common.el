;;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'buttercup)
(require 'org-incoming)

(setq org-incoming--test-dir (concat default-directory "test"))

(defvar org-incoming--test-base "" nil)

(defun file-name-concat (lhs rhs)
	 (concat (file-name-as-directory lhs) rhs)) 

(defun org-incoming--test-absfile (relfile)
	(file-name-concat org-incoming--test-base relfile))

(defun org-incoming--test-create-dir ()
	""
	(setq org-incoming--test-base (make-temp-file "org-incoming-test" 't))
	(message (format "Testdir: %s" org-incoming--test-base)))

(defun org-incoming--test-cleanup-dir ()
	""
	(delete-directory org-incoming--test-base 't))

(defun org-incoming--test-mkdir (reldir)
	(let ((absdir (org-incoming--test-absfile reldir)))
		(make-directory absdir 't)))

(defun org-incoming--test-prepare-file (srcdstpair)
	""
	(let ((source (nth 0 srcdstpair))
				(destination (nth 1 srcdstpair)))
		(message (format "%s -> %s" source destination))
		(let* ((testbase org-incoming--test-dir)
					 (fullsrc (file-name-concat testbase source))
					 (fulldst (org-incoming--test-absfile destination))
					 (dstdir (file-name-directory fulldst)))
			(make-directory dstdir 't)
			(copy-file fullsrc fulldst))))


(defun org-incoming--test-prepare-files (srcdstlst)
	""
	(-each srcdstlst 'org-incoming--test-prepare-file))
