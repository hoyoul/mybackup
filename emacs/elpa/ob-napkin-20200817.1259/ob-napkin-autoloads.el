;;; ob-napkin-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from ob-napkin.el

(autoload 'org-babel-execute:napkin "ob-napkin" "\
Execute a block of napkin code with BODY and PARAMS with Babel.
napkin tool will be invoked to generate the image.

(fn BODY PARAMS)")
(autoload 'org-babel-execute:napkin-puml "ob-napkin" "\
Execute a block of plantuml code with BODY and PARAMS with Babel.
napkin_plantuml tool will be invoked to generate the image.

(fn BODY PARAMS)")
(register-definition-prefixes "ob-napkin" '("ob-napkin-unload-function" "org-babel-"))

;;; End of scraped data

(provide 'ob-napkin-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; ob-napkin-autoloads.el ends here
