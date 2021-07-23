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
  :menu-entry '(?H "Export as Clean HTML" org-html-clean-export-to-html)
  :translate-alist
  '((link . org-html-clean-link)))

(defun org-html-clean-link (link desc info)
  "Transcode a LINK object from Org to HTML.

DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((home (when (plist-get info :html-link-home)
                 (org-trim (plist-get info :html-link-home))))
         (use-abs-url (plist-get info :html-link-use-abs-url))
         (link-org-files-as-html-maybe
          (function
           (lambda (raw-path info)
             "Treat links to `file.org' as links to `file.html', if needed.
           See `org-html-link-org-files-as-html'."
             (message "Calling link-org-files-as-html-maybe with %s" raw-path)
             (cond
              ((and org-html-link-org-files-as-html
                    (string= ".org"
                             (downcase (file-name-extension raw-path "."))))
               (concat (file-name-sans-extension raw-path)))
              (t
               raw-path)))))
         (type (org-element-property :type link))
         (raw-path (org-element-property :path link))
         ;; Ensure DESC really exists, or set it to nil.
         (desc (org-string-nw-p desc))
         (path
	  (cond
	   ((member type '("http" "https" "ftp" "mailto"))
	    (url-encode-url (concat type ":" raw-path)))
	   ((string= type "file")
	    ;; Treat links to ".org" files as ".html", if needed.
            (setq raw-path
                  (funcall link-org-files-as-html-maybe raw-path info))
            ;; If file path is absolute, prepend it with protocol
            ;; component - "file:".
            (cond
             ((file-name-absolute-p raw-path)
              (setq raw-path (concat "file:" raw-path)))
             ((and home use-abs-url)
              (setq raw-path (concat (file-name-as-directory home) raw-path))))
            ;; Add search option, if any.  A search option can be
            ;; relative to a custom-id or a headline title.  Any other
            ;; option is ignored.
            (let ((option (org-element-property :search-option link)))
              (cond ((not option) raw-path)
                    ((eq (aref option 0) ?#) (concat raw-path option))
                    ;; External fuzzy link: try to resolve it if path
                    ;; belongs to current project, if any.
                    ((eq (aref option 0) ?*)
                     (concat
                      raw-path
                      (let ((numbers
                             (org-publish-resolve-external-fuzzy-link
                              (org-element-property :path link) option)))
                        (and numbers (concat "#sec-"
                                             (mapconcat 'number-to-string
                                                        numbers "-"))))))
                    (t raw-path))))
           (t raw-path)))
         ;; Extract attributes from parent's paragraph.  HACK: Only do
         ;; this for the first link in parent (inner image link for
         ;; inline images).  This is needed as long as attributes
         ;; cannot be set on a per link basis.
         (attributes-plist
          (let* ((parent (org-export-get-parent-element link))
                 (link (let ((container (org-export-get-parent link)))
                         (if (and (eq (org-element-type container) 'link)
                                  (org-html-inline-image-p link info))
                             container
                           link))))
            (and (eq (org-element-map parent 'link 'identity info t) link)
                 (org-export-read-attribute :attr_html parent))))
         (attributes
          (let ((attr (org-html--make-attribute-string attributes-plist)))
            (if (org-string-nw-p attr) (concat " " attr) "")))
         protocol)
    (cond
     ;; Image file.
     ((and org-html-inline-images
           (org-export-inline-image-p link org-html-inline-image-rules))
      (org-html--format-image path attributes-plist info))
     ;; Radio target: Transcode target's contents and use them as
     ;; link's description.
     ((string= type "radio")
      (let ((destination (org-export-resolve-radio-link link info)))
        (if (not destination) desc
          (format "<a href=\"#%s\"%s>%s</a>"
                  (org-export-solidify-link-text
                   (org-element-property :value destination))
                  attributes desc))))
     ;; Links pointing to a headline: Find destination and build
     ;; appropriate referencing command.
     ((member type '("custom-id" "fuzzy" "id"))
      (let ((destination (if (string= type "fuzzy")
                             (org-export-resolve-fuzzy-link link info)
                           (org-export-resolve-id-link link info))))
        (case (org-element-type destination)
          ;; ID link points to an external file.
          (plain-text
           (let ((fragment (concat "ID-" path))
                 ;; Treat links to ".org" files as ".html", if needed.
                 (path (funcall link-org-files-as-html-maybe
                                destination info)))
             (format "<a href=\"%s#%s\"%s>%s</a>"
                     path fragment attributes (or desc destination))))
          ;; Fuzzy link points nowhere.
          ((nil)
           (format "<i>%s</i>"
                   (or desc
                       (org-export-data
                        (org-element-property :raw-link link) info))))
          ;; Link points to a headline.
          (headline
           (let ((href
                  ;; What href to use?
                  (cond
                   ;; Case 1: Headline is linked via it's CUSTOM_ID
                   ;; property.  Use CUSTOM_ID.
                   ((string= type "custom-id")
                    (org-element-property :CUSTOM_ID destination))
                   ;; Case 2: Headline is linked via it's ID property
                   ;; or through other means.  Use the default href.
                   ((member type '("id" "fuzzy"))
                    (format "sec-%s"
                            (mapconcat 'number-to-string
                                       (org-export-get-headline-number
                                        destination info) "-")))
                   (t (error "Shouldn't reach here"))))
                 ;; What description to use?
                 (desc
                  ;; Case 1: Headline is numbered and LINK has no
                  ;; description.  Display section number.
                  (if (and (org-export-numbered-headline-p destination info)
                           (not desc))
                      (mapconcat 'number-to-string
                                 (org-export-get-headline-number
                                  destination info) ".")
                    ;; Case 2: Either the headline is un-numbered or
                    ;; LINK has a custom description.  Display LINK's
                    ;; description or headline's title.
                    (or desc (org-export-data (org-element-property
                                               :title destination) info)))))
             (format "<a href=\"#%s\"%s>%s</a>"
                     (org-export-solidify-link-text href) attributes desc)))
          ;; Fuzzy link points to a target or an element.
          (t
           (let* ((path (org-export-solidify-link-text path))
                  (org-html-standalone-image-predicate 'org-html--has-caption-p)
                  (number (cond
                           (desc nil)
                           ((org-html-standalone-image-p destination info)
                            (org-export-get-ordinal
                             (org-element-map destination 'link
                               'identity info t)
                             info 'link 'org-html-standalone-image-p))
                           (t (org-export-get-ordinal
                               destination info nil 'org-html--has-caption-p))))
                  (desc (cond (desc)
                              ((not number) "No description for this link")
                              ((numberp number) (number-to-string number))
                              (t (mapconcat 'number-to-string number ".")))))
             (format "<a href=\"#%s\"%s>%s</a>" path attributes desc))))))
     ;; Coderef: replace link with the reference name or the
     ;; equivalent line number.
     ((string= type "coderef")
      (let ((fragment (concat "coderef-" path)))
        (format "<a href=\"#%s\"%s%s>%s</a>"
                fragment
                (org-trim
                 (format (concat "class=\"coderef\""
                                 " onmouseover=\"CodeHighlightOn(this, '%s');\""
                                 " onmouseout=\"CodeHighlightOff(this, '%s');\"")
                         fragment fragment))
                attributes
                (format (org-export-get-coderef-format path desc)
                        (org-export-resolve-coderef path info)))))
     ;; Link type is handled by a special function.
     ((org-export-custom-protocol-maybe link desc 'html info))
     ;; External link with a description part.
     ((and path desc) (format "<a href=\"%s\"%s>%s</a>" path attributes desc))
     ;; External link without a description part.
     (path (format "<a href=\"%s\"%s>%s</a>" path attributes path))
     ;; No path, only description.  Try to do something useful.
     (t (format "<i>%s</i>" desc)))))

(defun html-clean-create-index-folder (orig-fun &rest args)
  "Patch `org-export-output-file-name' to return my-post/index.html.
Argument ORIG-FUN the function being advised.
Optional argument ARGS the arguments to ORIG-FUN."

  (let* ((orig-output (apply orig-fun args))
         (new-output (concat (file-name-sans-extension orig-output) "/index.html")))
    (if (equal (file-name-nondirectory orig-output) "index.html")
        orig-output
      (make-directory (file-name-directory new-output) t)
      new-output)))


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
