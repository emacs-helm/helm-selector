;;; helm-selector.el --- Helm buffer selector -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-helm/helm-selector
;; Version: 0.5
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
;; Helm Selector is a collection of Helm (https://emacs-helm.github.io/helm/)
;; helper functions for convenient buffer selection.
;;
;; It is especially helpful to create Helm sessions to navigate buffers of a
;; given mode in a "do what I mean" fashion:
;;
;; - If current buffer is not of mode X, switch to last buffer of mode X, or
;;   create one if none exists.
;; - If current buffer is of mode X, show a Helm session of all buffers in mode X.
;;
;; In the Helm session, it's also possible to input an arbitrary name which will be
;; used for the creation of a new buffer of mode X.
;;
;; Helm Selector comes with a bunch of predefined selectors which should be
;; autoloaded.  Here follows an example setup to bind the Info and the shell
;; selector:
;;
;; (require 'helm-selector)
;; (global-set-key (kbd "C-h i") 'helm-selector-info)
;; (global-set-key (kbd "s-RET") 'helm-selector-shell)
;; (global-set-key (kbd "s-S-RET") 'helm-selector-shell-other-window)
;;
;; Calling `helm-selector-shell' with a prefix argument is equivalent to
;; `helm-selector-shell-other-window'.

;;; Code:

(require 'helm)
(require 'helm-mode)
(require 'helm-buffers)
(require 'cl-lib)

(defun helm-selector-major-modes-predicate (&rest modes)
  "Return a predicate.
The predicate returns non-nil if current buffer derives from one
of the MODES."
  (lambda (buffer)
    (with-current-buffer buffer
      (cl-find-if #'derived-mode-p modes))))

(defun helm-selector--dummy-source (&optional mode make-buffer-fn)
  "Helm source to create a new buffer.
MAKE-BUFFER-FN takes the Helm input as argument for the new buffer name and
returns the new buffer.
If MAKE-BUFFER-FN is omitted create buffer in MODE (query the user if nil).
See `helm-source-buffer-not-found'."
  (helm-build-dummy-source
      "Create buffer"
    :action
    (list
     (cons "Create buffer (C-u choose mode)"
           (lambda (candidate)
             (if (not make-buffer-fn)
                 (let ((new-buffer-mode
                        (or (and helm-current-prefix-arg
                                 (intern-soft (helm-comp-read
                                               "Major-mode: "
                                               helm-buffers-favorite-modes)))
                            mode
                            (cl-loop for (regex . mode) in auto-mode-alist
                                     when (string-match regex candidate)
                                     return mode)))
                       (buffer (get-buffer-create candidate)))
                   (if new-buffer-mode
                       (with-current-buffer buffer (funcall new-buffer-mode))
                     (set-buffer-major-mode buffer))
                   (switch-to-buffer buffer))
               (let ((new-buffer (save-window-excursion
                                   (funcall make-buffer-fn candidate)
                                   (current-buffer))))
                 (switch-to-buffer new-buffer))))))))

(defun helm-selector--filter-buffers (predicate)
  "Return list of buffer names accepted by PREDICATE.
If current buffer would come first, it's listed last."
  (let ((filtered-buffers (cl-remove-if-not predicate (buffer-list))))
    (mapcar #'buffer-name
            (if (eq (cl-first filtered-buffers) (current-buffer))
                (append (cl-rest filtered-buffers) (list (current-buffer)))
              filtered-buffers))))

(cl-defun helm-selector--default-source (name
                                         &key
                                         predicate)
  (helm-make-source (format "%s buffers" name) 'helm-source-buffers
    :buffer-list (lambda ()
                   (helm-selector--filter-buffers predicate))))

(cl-defun helm-selector--default-sources (name
                                          &key
                                          predicate
                                          make-buffer-fn
                                          extra-sources)
  (list (helm-selector--default-source name :predicate predicate)
        (let ((mode (intern (format "%s-mode" (downcase name)))))
          (unless (fboundp mode)
            ;; For cased mode names like `Info-mode'.
            (setq mode (intern (format "%s-mode" name))))
          (when (fboundp mode)
            (helm-selector--dummy-source mode make-buffer-fn)))
        extra-sources))

(cl-defun helm-selector--default-helm (name
                                       &key
                                       predicate
                                       make-buffer-fn
                                       extra-sources)

  (helm
   :sources (helm-selector--default-sources
             name
             :predicate predicate
             :make-buffer-fn make-buffer-fn
             :extra-sources extra-sources)
   :buffer (format "*helm-%s-buffers*" name)))

;;;###autoload
(cl-defun helm-selector (name
                         &key
                         (predicate (error "A predicate must be given"))
                         helm-sources
                         (make-buffer-fn (error "A buffer creation function is required"))
                         extra-sources
                         use-follow-p)
  "Create an interactive buffer switcher NAME.
When current buffer does not satisfy PREDICATE, switch to the last-buffer buffer
that does.
When current buffer does satisfy it, show a Helm buffer:

- HELM-SOURCES if non-nil.
- A default Helm lister of all buffers that satisfy PREDICATE otherwise.

In the Helm session, it's also possible to input an arbitrary
name which will be passed as first argument to MAKE-BUFFER-FN for
the creation of a new buffer of mode X.

When there is no buffer in mode X, MAKE-BUFFER-FN is invoked
without argument to create a buffer.

When provided, EXTRA-SOURCES are appended to the default Helm lister.
USE-FOLLOW-P enables follow-mode for the default Helm lister."
  (let ((other-window-p current-prefix-arg)
        (buffer-source-name (format "%s buffers" name)))
    (when use-follow-p
      (add-to-list 'helm-source-names-using-follow buffer-source-name))
    (if (funcall predicate (current-buffer))
        (if helm-sources
            (funcall helm-sources)
          (helm-selector--default-helm name
                                       :predicate predicate
                                       :make-buffer-fn make-buffer-fn
                                       :extra-sources extra-sources))
      (let ((last-buffer (cl-find-if predicate (buffer-list))))
        (cond
         ((and last-buffer (get-buffer-window last-buffer))
          (select-window (get-buffer-window last-buffer)))
         (t (setq last-buffer (or last-buffer
                                  (save-window-excursion
                                    (funcall make-buffer-fn)
                                    (current-buffer))))
            (funcall (if other-window-p
                         'switch-to-buffer-other-window
                       'switch-to-buffer)
                     last-buffer)))))))

(provide 'helm-selector)
;;; helm-selector.el ends here
