;;; helm-selector-sly.el --- Helm SLY buffer selector -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-helm/helm-selector
;; Version: 0.4
;; Package-Requires: ((emacs "26.1") (helm "3"))

;; This file is not part of GNU Emacs.

;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>

;;; Commentary:
;;
;; Support file for helm-selector.

;;; Code:

(require 'helm-selector)
(require 'sly nil :noerror)
(require 'helm-sly nil :noerror)

(declare-function sly "sly")
(declare-function sly-current-connection "sly")
(declare-function sly-mrepl--find-buffer "sly-mrepl")
(declare-function helm-sly-mini "helm-sly")

;;;###autoload
(defun helm-selector-sly ()
  "Helm for `sly' buffers."
  (interactive)
  (helm-selector
   "SLY-REPL"
   :predicate (lambda (buffer)
                (eq buffer (ignore-errors
                             (sly-mrepl--find-buffer (sly-current-connection)))))
   :make-buffer-fn (lambda ()
                     (interactive)
                     (if (< 1 (length sly-lisp-implementations))
                         (let ((current-prefix-arg '-))
                           (call-interactively #'sly))
                       (sly)))
   :helm-sources #'helm-sly-mini))

;;;###autoload
(defun helm-selector-sly-other-window ()
  "Like `helm-selector-sly' but raise buffer in other window."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'helm-selector-sly)))

(provide 'helm-selector-sly)
;;; helm-selector-sly.el ends here
