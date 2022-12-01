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

(describe "Process a single file in roam mode"
					:var ((org-incoming-dirs `((:source ,(org-incoming--test-absfile "incoming") :target ,(org-incoming--test-absfile "processed")))))
					
					(before-all
					 (org-incoming--test-create-dir)
					 (org-incoming--test-prepare-files '(("data/testpdf.pdf" "incoming/testpdf.pdf")))
					 (org-incoming--test-mkdir "processed"))

					(after-all
					 (org-incoming--test-cleanup-dir))

					(it "works and has an ID"
							(setq org-incoming-dirs `((:source ,(org-incoming--test-absfile "incoming")
																								 :target ,(org-incoming--test-absfile "processed")
																								 :use-roam 't)))
							
							;; Start the session
							(org-incoming-start)

							;; Make sure windows are correct
							(let* ((current-win (selected-window))
										 (current-buf (window-buffer current-win))
										 (window-count (length (window-list)))
										 (other-win (nth 1 (window-list)))
										 (other-buf (window-buffer other-win)))
								
								(expect (buffer-name current-buf) :to-equal "*org-incoming-query*")
								(expect (buffer-name other-buf) :to-equal "testpdf.pdf"))

							;; Cursor should be in title field. Enter a title
							(insert "Test Title")
							;; Move to date field and enter a date
							(widget-forward 1)
							(insert "2022-11-26")
							
							;; Complete query phase
							(org-incoming-complete)
							
							;; We should be in annotation phase now
							;; Make sure windows are correct
							(let* ((current-win (selected-window))
										 (current-buf (window-buffer current-win))
										 (window-count (length (window-list)))
										 (other-win (nth 1 (window-list)))
										 (other-buf (window-buffer other-win)))
								(expect (buffer-name current-buf) :to-equal "annotation.org")
								(expect (buffer-name other-buf) :to-equal "testpdf.pdf"))

							;; Since we created a roam node, the first line should be ":PROPERTIES:"
							(goto-char 0)
							(end-of-line)
							(let* ((eol-point (point))
										 (line-text (substring (buffer-string) 0 (- eol-point 1))))
								(expect line-text :to-equal ":PROPERTIES:"))

							;; Second line should start with ":ID:"
							;; TODO: Why do I need to shift point to the left by one?
							(goto-char 0)
							(forward-line 1)
							(beginning-of-line)
							(let* ((line-text (substring (buffer-string) (- (point) 1) (+ (point) 3))))
								(expect line-text :to-equal ":ID:"))
							
							;; Just complete the phase
							(org-incoming-complete)
							
							;; Make sure the files are where they belong
							(let ((expected-org (org-incoming--test-absfile "processed/2022-11-26_Test_Title.org"))
										(expected-pdf (org-incoming--test-absfile "processed/pdfs/2022-11-26_Test_Title.pdf")))
								(expect (file-exists-p expected-org) :to-be-truthy)
								(expect (file-exists-p expected-pdf) :to-be-truthy))))
				
