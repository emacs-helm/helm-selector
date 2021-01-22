;;; helm-selector-cider.el --- Helm CIDER buffer selector -*- lexical-binding: t; -*-

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
(require 'cider nil :noerror)

(declare-function cider "cider")

;;;###autoload
(defun helm-selector-cider ()
  "Helm for `cider' buffers."
  (interactive)
  (helm-selector
   "CIDER-REPL"
   :predicate (helm-selector-major-modes-predicate 'cider-repl-mode)
   :make-buffer-fn #'cider
   :use-follow-p t))

;;;###autoload
(defun helm-selector-cider-other-window ()
  "Like `helm-selector-cider' but raise buffer in other window."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'helm-selector-cider)))

(provide 'helm-selector-cider)
;;; helm-selector-cider.el ends here
