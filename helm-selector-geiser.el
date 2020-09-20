;;; helm-selector-geiser.el --- Helm Geiser buffer selector -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-helm/helm-selector
;; Version: 0.3
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

(declare-function geiser-repl--repl-name "ext:geiser-repl")
(declare-function run-geiser "ext:geiser-repl")

(defvar geiser-repl-buffer-name-function)

;;;###autoload
(defun helm-selector-geiser ()
  "Helm for `geiser' buffers."
  (interactive)
  (require 'geiser-impl)
  (require 'geiser-repl)
  (helm-selector
   "Geiser-REPL"
   :predicate (helm-selector-major-modes-predicate 'geiser-repl-mode)
   :make-buffer-fn (lambda (&optional name)
                     (let ((geiser-repl-buffer-name-function
                            (lambda (impl)
                              (format "* %s%s *" (geiser-repl--repl-name impl)
                                      (if name (concat "-" name) "")))))
                       (call-interactively 'run-geiser)))
   :use-follow-p t))

;;;###autoload
(defun helm-selector-geiser-other-window ()
  "Like `helm-selector-geiser' but raise buffer in other window."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'helm-selector-geiser)))

(provide 'helm-selector-geiser)
;;; helm-selector-geiser.el ends here
