;;; helm-selector-slime.el --- Helm SLIME buffer selector -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-helm/helm-selector
;; Version: 0.6
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
(require 'slime nil :noerror)
(require 'helm-slime nil :noerror)

(declare-function slime "slime")
(declare-function slime-output-buffer "slime")
(declare-function helm-slime-mini "helm-slime")

;;;###autoload
(defun helm-selector-slime ()
  "Helm for `slime' buffers."
  (interactive)
  (helm-selector
   "SLIME-REPL"
   :predicate (lambda (buffer)
                (eq buffer (ignore-errors (slime-output-buffer))))
   :make-buffer-fn #'slime
   :helm-sources #'helm-slime-mini))

;;;###autoload
(defun helm-selector-slime-other-window ()
  "Like `helm-selector-slime' but raise buffer in other window."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'helm-selector-slime)))

(provide 'helm-selector-slime)
;;; helm-selector-slime.el ends here
