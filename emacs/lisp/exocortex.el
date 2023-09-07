;; exocortex.el --- Org export backend for html used for an Exocortex

;;; Commentary
;; This mostly is a copy of ox-jujutsu-site.el
;; (https://github.com/grc/jujutsu-website/blob/master/elisp/ox-jujutsu-site.el)

(require 'ox-html)
(require 's)

(defvar org-exocortex-nav-items
  '(("/index.html" . "Index"))
  "List of navigation menu URLs and their associated labels.")

;;; Define backend
(org-export-define-derived-backend 'exocortex 'html
  :translate-alist '((template . org-exocortex-site-template))
  :menu-entry '(?E "Exocortex" org-html-export-to-html))

(defun org-exocortex-site-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string. INFO is a plist
holding export options."
  (concat
   (when (and (not (org-html-html5-p info)) (org-html-xhtml-p info))
     (let* ((xml-declaration (plist-get info :html-xml-declaration))
	    (decl (or (and (stringp xml-declaration) xml-declaration)
		      (cdr (assoc (plist-get info :html-extension)
				  xml-declaration))
		      (cdr (assoc "html" xml-declaration))
		      "")))
       (when (not (or (not decl) (string= "" decl)))
	 (format "%s\n"
		 (format decl
			 (or (and org-html-coding-system
				  (fboundp 'coding-system-get)
				  (coding-system-get org-html-coding-system 'mime-charset))
			     "utf-8"))))))
   (org-html-doctype info)
   "\n"
   (concat "<html"
	   (cond ((org-html-xhtml-p info)
		  (format
		   " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
		   (plist-get info :language) (plist-get info :language)))
		 ((org-html-html5-p info)
		  (format " lang=\"%s\"" (plist-get info :language))))
	   ">\n")

   (org-exocortex-site-head contents info)
   (org-exocortex-site-body contents
                          (plist-get info :input-file)
                          (plist-get info :base-directory))
   "</html>\n"))

(defun org-exocortex-site-head (contents info)
  "Return the HTML head for the web page."
  (let ((file (plist-get info :input-file))
        (directory (plist-get info :base-directory)))
    (concat "<head>\n"
            "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
            (org-html--build-meta-info info)
            (org-html--build-head info)
            "<link href=\"https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/css/bootstrap.min.css\" rel=\"stylesheet\" integrity=\"sha384-EVSTQN3/azprG1Anm3QDgpJLIm9Nao0Yz1ztcQTwFspd3yD65VohhpuuCOmLASjC\" crossorigin=\"anonymous\">\n"
            "<link href=\"https://fonts.googleapis.com/icon?family=Material+Icons\" rel=\"stylesheet\">\n"
            (if (equal file org-roam-index-file)
                "<link href=\"css/style.css\" rel=\"stylesheet\">\n"
                "<link href=\"../css/style.css\" rel=\"stylesheet\">\n"
                )
            "</head>\n")))

(defun org-exocortex-site-body (contents file directory)
  ""
  (concat "<body>\n"
          (org-html--build-pre/postamble 'preamble info)

          ;; Document title.
          "<div class=\"container\">"
          (when (plist-get info :with-title)
            (let ((title (and (plist-get info :with-title)
		                      (plist-get info :title)))
	              (subtitle (plist-get info :subtitle)))
              (when title
                (format "<div class=\"row\"><h1 class=\"title\">%s</h1></div>" (org-export-data title info)))))

          ;; Contents
          "<div class=\"row\">\n"
          contents
          "</div>\n"
          "</div>\n"
          
          ;; Postamble.
          (org-html--build-pre/postamble 'postamble info)
          ;; Possibly use the Klipse library live code blocks.
          (org-exocortex-bootstrap-scripts file)
          ;; Closing document.
          "</body>\n"
          ))


(defun org-exocortex-bootstrap-scripts (file)
  "Javascript files required for Exocortex."
  (concat 
     "<script src=\"https://cdn.jsdelivr.net/npm/bootstrap@5.0.2/dist/js/bootstrap.bundle.min.js\" integrity=\"sha384-MrcW6ZMFYlzcLA8Nl+NtUVF0sA7MsXsP1UyJoMp4YLEuNSfAP+JcXn/tWtIaxVXM\" crossorigin=\"anonymous\"></script>\n"
     "<script src=\"https://code.jquery.com/jquery-3.6.0.min.js\" integrity=\"sha256-/xUj+3OJU5yExlq6GSYGSHk7tPXikynS7ogEvDej/m4=\" crossorigin=\"anonymous\"></script>\n"
     "<script src=\"https://polyfill.io/v3/polyfill.min.js?features=es6\"></script>"
     "<script id=\"MathJax-script\" async src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js\"></script>"
     (if (equal file org-roam-index-file)
         "<script src=\"js/tweaks.js\"></script>\n"
       "<script src=\"../js/tweaks.js\"></script>\n"
       ))
  )

(defun org-exocortex-publish-to-html (plist filename pub-dir)
  "Publish an org file to Exocortex HTML.

FILENAME is the filename of the Org file to be published. PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (org-publish-org-to 'exocortex filename
                      (concat "."  (or (plist-get plist :html-extension)
				      org-html-extension
				      "html"))
                      plist
                      pub-dir))

(provide 'exocortex)
