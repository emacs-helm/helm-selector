;;; helm-selector-org.el --- Helm Org buffer selector -*- lexical-binding: t; -*-

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
(require 'subr-x)
(require 'org)

;;;###autoload
(defun helm-selector-org ()
  "Helm for `org' buffers."
  (interactive)
  (helm-selector
   "Org"
   :predicate (lambda (buffer)
                (when (buffer-file-name buffer)
                  (member (file-truename (buffer-file-name buffer))
                          (mapcar #'file-truename org-agenda-files))))
   :make-buffer-fn (lambda (&optional name)
                     (if (not name)
                         ;; No .org buffer opened yet, let's open the first agenda:
                         (find-file (car (org-agenda-files)))
                       (let* ((org-ext?
                               (lambda (name)
                                 (and-let* ((name name)
                                            (ext (file-name-extension name)))
                                   (member (downcase ext) '("org" "gpg")))))
                              (file-name
                               (expand-file-name
                                (if (funcall org-ext? name)
                                    name
                                  (concat name ".org"))
                                (file-name-directory (or (car org-agenda-files)
                                                         (buffer-file-name))))))
                         (add-to-list 'org-agenda-files file-name)
                         (find-file file-name))))
   :extra-sources (helm-make-source "Org agenda files" 'helm-source-ffiles
                    :candidates (lambda () org-agenda-files))
   :use-follow-p t))

;;;###autoload
(defun helm-selector-org-other-window ()
  "Like `helm-selector-org' but raise buffer in other window."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'helm-selector-org)))

(provide 'helm-selector-org)
;;; helm-selector-org.el ends here
