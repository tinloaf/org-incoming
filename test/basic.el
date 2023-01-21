;;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'buttercup)
(require 'org-incoming)

(load "./test/test-common.el")

(describe "Process a single file with default settings"
					:var ((org-incoming-dirs `((:source ,(org-incoming--test-absfile "incoming") :target ,(org-incoming--test-absfile "processed")))))
					
					(before-all
					 (org-incoming--test-create-dir)
					 (org-incoming--test-prepare-files '(("data/testpdf.pdf" "incoming/testpdf.pdf")))
					 (org-incoming--test-mkdir "processed"))

					(after-all
					 (org-incoming--test-cleanup-dir))

					(it "runs a basic org-incoming session"
							(setq org-incoming-dirs `((:source ,(org-incoming--test-absfile "incoming") :target ,(org-incoming--test-absfile "processed"))))
							
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

							;; Just complete the phase
							(org-incoming-complete)
							
							;; Make sure the files are where they belong
							(let ((expected-org (org-incoming--test-absfile "processed/2022-11-26_Test_Title.org"))
										(expected-pdf (org-incoming--test-absfile "processed/pdfs/2022-11-26_Test_Title.pdf")))
								(expect (file-exists-p expected-org) :to-be-truthy)
								(expect (file-exists-p expected-pdf) :to-be-truthy))))
				
