;;; helm-selector-mu4e.el --- Helm mu4e buffer selector -*- lexical-binding: t; -*-

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
(require 'mu4e nil :noerror)

(declare-function mu4e "mu4e")
(declare-function mu4e-conversation--buffer-p  "mu4e-conversation")

;;;###autoload
(defun helm-selector-mu4e ()
  "Helm for `mu4e' buffers."
  (interactive)
  (helm-selector
   "mu4e"
   :predicate (lambda (buffer)
                (with-current-buffer buffer
                  (or
                   (derived-mode-p 'mu4e-main-mode)
                   (derived-mode-p 'mu4e-headers-mode)
                   (derived-mode-p 'mu4e-view-mode)
                   (derived-mode-p 'mu4e-compose-mode)
                   (when (require 'mu4e-conversation nil 'noerror)
                     (mu4e-conversation--buffer-p (list buffer))))))
   :make-buffer-fn #'mu4e
   :use-follow-p t))

;;;###autoload
(defun helm-selector-mu4e-other-window ()
  "Like `helm-selector-mu4e' but raise buffer in other window."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'helm-selector-mu4e)))

(provide 'helm-selector-mu4e)
;;; helm-selector-mu4e.el ends here
