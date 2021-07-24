;;; ox-html-clean.el --- A clean HTML export for org-export

;;; Commentary:
;;

;;; Code:

(require 'ox)
(require 'ox-publish)
(require 'ox-html)
(require 'format-spec)

(org-export-define-derived-backend
    'html-clean 'html
  :menu-entry '(?H "Export as Clean HTML" org-html-clean-export-to-html))

(defun html-clean-create-index-folder (orig-fun &rest args)
  "Patch `org-export-output-file-name' to return my-post/index.html.
Argument ORIG-FUN the function being advised.
Optional argument ARGS the arguments to ORIG-FUN."

  (let* ((orig-output (apply orig-fun args))
	 (new-output (concat (file-name-sans-extension orig-output) "/index.html")))
    (if (equal (file-name-nondirectory orig-output) "index.html")
	orig-output
      (if (equal (file-name-nondirectory (directory-file-name (file-name-directory orig-output))) (file-name-base orig-output))
	  (concat (directory-file-name (file-name-directory orig-output)) "/index.html")

	(make-directory (file-name-directory new-output) t)
	new-output))))

;;;###autoload
(defun org-html-clean-export-to-html
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a HTML file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

When optional argument BODY-ONLY is non-nil, only write code
between \"<body>\" and \"</body>\" tags.

EXT-PLIST, when provided, is a property list with external
parameters overriding Org default settings, but still inferior to
file-local settings.

Return output file's name."
  (interactive)
  (advice-add 'org-export-output-file-name
              :around #'html-clean-create-index-folder)
  (let* ((extension (concat "." org-html-extension))
         (file (org-export-output-file-name extension subtreep))
         (org-export-coding-system org-html-coding-system))
    (org-export-to-file 'html file
      async subtreep visible-only body-only ext-plist))
  (advice-remove 'org-export-output-file-name
                 #'html-clean-create-index-folder))


;;;###autoload
(defun org-html-clean-publish-to-html (plist filename pub-dir)
  "Publish an org file to HTML.

PLIST is the property list for the given project.  FILENAME is
the filename of the Org file to be published.  PUB-DIR is the
publishing directory.

Return output file name."
  (advice-add 'org-export-output-file-name
              :around #'html-clean-create-index-folder)
  (org-publish-org-to 'html-clean filename
                      (concat "." (or (plist-get plist :html-extension)
                                      org-html-extension "html"))
                      plist pub-dir)
  (advice-remove 'org-export-output-file-name
                 #'html-clean-create-index-folder))

(provide 'ox-html-clean)

(provide 'ox-html-clean)

;;; ox-html-clean.el ends here
