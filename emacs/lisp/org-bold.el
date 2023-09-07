(defun onlybold-bold (bold contents info)
  (concat "[bold]" contents info "[endbold]"))

(defun onlybold-section (section contents info)
  (concat "[section]" contents "[endsection]"))

(defun onlybold-paragraph (paragraph contents info)
  (concat "[para]" contents "[endpara]"))

(org-export-define-backend 'onlybold
  '((bold . onlybold-bold)
    (section . onlybold-section)
    (paragraph . onlybold-paragraph)))

(defun onlybold-export ()
  (interactive)
  (org-export-to-buffer 'onlybold "*onlybold*"))

(provide 'org-bold)
