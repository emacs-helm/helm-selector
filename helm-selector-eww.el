;;; helm-selector-eww.el --- Helm EWW buffer selector -*- lexical-binding: t; -*-

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
(require 'eww)
(require 'thingatpt)
(require 'helm-eww nil t)

(declare-function helm-eww "helm-eww")

;;;###autoload
(defun helm-selector-eww ()
  "Helm for `eww' buffers."
  (interactive)
  (helm-selector
   "eww"
   :predicate (helm-selector-major-modes-predicate 'eww-mode)
   :make-buffer-fn (lambda (&optional _)
                     (interactive)
                     (if (thing-at-point-url-at-point)
                         ;; This only works fine if no EWW buffer is up.
                         (call-interactively #'eww)
                       (helm-eww)))
   :helm-sources #'helm-eww
   :use-follow-p t))

;;;###autoload
(defun helm-selector-eww-other-window ()
  "Like `helm-selector-eww' but raise buffer in other window."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'helm-selector-eww)))

(provide 'helm-selector-eww)
;;; helm-selector-eww.el ends here
