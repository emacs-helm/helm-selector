;;; helm-selector-shell.el --- Helm shell buffer selector -*- lexical-binding: t; -*-

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
(require 'helm-files)                   ; For `helm-ff-preferred-shell-mode'.
(require 'shell)
(require 'eshell)

;;;###autoload
(defun helm-selector-shell ()
  "Helm for `shell' or `eshell' buffers.
Default shell type is chosen with `helm-ff-preferred-shell-mode'."
  (interactive)
  (let ((prefer-eshell (eq helm-ff-preferred-shell-mode 'eshell-mode)))
    (helm-selector
     "shell"
     :predicate (helm-selector-major-modes-predicate 'shell-mode 'eshell-mode)
     :make-buffer-fn (lambda (&optional name)
                       (if prefer-eshell
                           (eshell 'new)
                         (shell (generate-new-buffer
                                 (generate-new-buffer-name "*shell*"))))
                       (when name
                         (rename-buffer (format "*%sshell<%s>*"
                                                (if prefer-eshell "e" "")
                                                name)
                                        'unique)))
     :use-follow-p t)))

;;;###autoload
(defun helm-selector-shell-other-window ()
  "Like `helm-selector-shell' but raise buffer in other window."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively #'helm-selector-shell)))

(provide 'helm-selector-shell)
;;; helm-selector-shell.el ends here
