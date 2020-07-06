;;; helm-selector-notmuch.el --- Helm notmuch buffer selector -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-helm/helm-selector
;; Version: 0.1
;; Package-Requires: ((emacs "25") (helm "3"))

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

;;; Code:

(require 'helm-selector)
(require 'notmuch)

;;;###autoload
(defun helm-selector-notmuch ()
  "Helm for `notmuch' buffers."
  (interactive)
  (helm-selector
   "notmuch"
   :predicate #'notmuch-interesting-buffer
   :make-buffer-fn #'notmuch-hello
   :use-follow-p t))

;;;###autoload
(defun helm-selector-notmuch-other-window ()
  "Like `helm-selector-notmuch' but raise buffer in other window."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'helm-selector-notmuch)))

(provide 'helm-selector-notmuch)
;;; helm-selector-notmuch.el ends here
