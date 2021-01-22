;;; helm-selector-magit.el --- Helm Magit buffer selector -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Pierre Neidhardt

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
(require 'magit nil :noerror)

(declare-function magit-repository-local-repository "magit-mode")
(declare-function magit-repos-alist "magit-repos")
(declare-function magit-status-setup-buffer "magit-status")
(defvar magit--default-directory)

(defun helm-selector-magit--buffer-repo-list ()
  (cl-delete-duplicates
   (mapcar (lambda (buffer)
             (with-current-buffer buffer
               (file-truename magit--default-directory)))
           (cl-remove-if-not (lambda (buffer)
                               (with-current-buffer buffer
                                 (derived-mode-p 'magit-mode)))
                             (buffer-list)))
   :test #'string=))

(defun helm-selector-magit--buffer-repo-list-sorted ()
  "Like `helm-selector-magit--buffer-repo-list' but current
repository is listed last."
  (let ((repo (file-truename (magit-repository-local-repository))))
    (if repo
        (cons repo
              (delete repo
                      (mapcar #'file-truename
                              (helm-selector-magit--buffer-repo-list))))
      (helm-selector-magit--buffer-repo-list))))

(defun helm-selector-magit--buffer-p (directory buffer)
  (let* ((directory (or directory magit--default-directory))
         (directory (when directory (file-truename directory))))
    (with-current-buffer buffer
      (and
       (derived-mode-p 'magit-mode)
       (or (null directory)
           (and magit--default-directory
                (string= directory
                         (file-truename magit--default-directory))))))))

(defun helm-selector-magit--make-repo-source (repo)
  (helm-selector--default-source
   (file-name-base (directory-file-name repo))
   :predicate (apply-partially #'helm-selector-magit--buffer-p repo)))

;;;###autoload
(defun helm-selector-magit ()
  "Helm for `magit' buffers of the current repository."
  (interactive)
  (helm-selector
   "Magit"
   :predicate (apply-partially #'helm-selector-magit--buffer-p
                               (magit-repository-local-repository))
   :helm-sources
   (lambda ()
     (helm
      :sources (append
                (mapcar #'helm-selector-magit--make-repo-source
                        (helm-selector-magit--buffer-repo-list-sorted))
                (list (helm-build-sync-source "Magit repositories"
                        :candidates (magit-repos-alist)
                        :action (helm-make-actions
                                 "Repository status"
                                 #'magit-status-setup-buffer))))
      :buffer "Magit buffers"))
   :make-buffer-fn (lambda ()
                     (if (magit-repository-local-repository)
                         (magit-status-setup-buffer)
                       (let ((current-prefix-arg t))
                         (call-interactively #'magit-status))))))

;;;###autoload
(defun helm-selector-magit-other-window ()
  "Like `helm-selector-magit' but raise buffer in other window."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'helm-selector-magit)))

(provide 'helm-selector-magit)
;;; helm-selector-magit.el ends here
