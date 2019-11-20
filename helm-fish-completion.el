;;; helm-fish-completion.el --- helm interface for fish completion -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-helm/helm-fish-completion
;; Version: 0.2
;; Package-Requires: ((emacs "25") (helm "3") (fish-completion "1.2"))

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
;; <http://www.gnu.org/licenses/>

;;; Commentary:
;;
;; You can invoke Helm Fish completion with the ~helm-fish-completion~ command.
;;
;; To replace completion in Eshell and =M-x shell=, simply rebind the TAB key:
;;
;; (when (require 'helm-fish-completion nil 'noerror)
;;   (define-key shell-mode-map (kbd "<tab>") 'helm-fish-completion)
;;   (setq helm-esh-pcomplete-build-source-fn #'helm-fish-completion-make-eshell-source))
;;
;; `fish-completion-mode' must be disabled.

;;; Code:

(require 'esh-mode)
(require 'helm)
(require 'helm-buffers)
(require 'helm-eshell)
(require 'fish-completion)

(defgroup helm-fish-completion nil
  "Emacs Helm interface for fish completion"
  :group 'helm)

(defcustom helm-fish-completion-length 40
  "Length of completion column before displaying the descriptions."
  :group 'helm-fish-completion
  :type 'integer)

(defun helm-fish-completion-toggle-desc ()
  "Toggle display of description column."
  (interactive)
  (with-helm-alive-p
    (let* ((buf (helm-get-selection))
           ;; `helm-buffer--get-preselection' uses `helm-buffer-max-length'.
           (helm-buffer-max-length helm-fish-completion-length)
           (preselect (and (bufferp buf) (helm-buffer--get-preselection buf))))
      (setq helm-buffer-details-flag (not helm-buffer-details-flag))
      ;; TODO: Why is `helm-force-update' necessary to update the buffer live?
      ;; It is not the case for `helm-buffers-list'.
      (if (bufferp buf)
          (helm-force-update (lambda ()
                               (helm-awhile (re-search-forward preselect nil t)
                                 (helm-mark-current-line)
                                 (when (equal buf (helm-get-selection))
                                   (cl-return t)))))
        (helm-force-update)))))
(put 'helm-fish-completion-toggle-desc 'helm-only t)

(defvar helm-fish-completion-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-]") 'helm-fish-completion-toggle-desc)
    map)
  "Keymap for browser source in Helm.")

(defun helm-fish-completion-complete (raw-prompt)
  "Complete RAW-PROMPT (any string) using the fish shell.
Fall back on bash with `fish-completion--maybe-use-bash'."
  (let* ((comp-list-with-desc (fish-completion--list-completions-with-desc raw-prompt))
         (comp-list (mapcar (lambda (e)
                              (car (split-string e "\t")))
                            (split-string comp-list-with-desc "\n" t)))
         (bash-list (fish-completion--maybe-use-bash comp-list)))
    (if (eq comp-list bash-list)
        (mapcar (lambda (e)
                  (let ((pair (split-string e "\t")))
                    (unless (cadr pair)
                      (setcdr pair '("")))
                    (cons (format (concat "%-" (number-to-string helm-fish-completion-length) "s  %s")
                                  (car pair)
                                  (if helm-buffer-details-flag
                                      (propertize (cadr pair) 'face 'helm-buffer-process)
                                    ""))
                          (car pair))))
                (split-string
                 comp-list-with-desc "\n" t))
      comp-list)))

(defun helm-fish-completion-shell-complete ()
  "Complete `shell' or `eshell' prompt with `fish-completion-complete'.
If we are in a remote location, use the old completion function instead,
since we rely on a local fish instance to suggest the completions."
  (unless (file-remote-p default-directory)
    (with-helm-current-buffer
      (helm-fish-completion-complete (buffer-substring-no-properties
                                      (save-excursion (if (eq major-mode 'shell-mode)
                                                          (comint-bol)
                                                        (eshell-bol))
                                                      (point))
                                      (point))))))

(defcustom helm-fish-completion-actions
  '(("Insert completion" . helm-ec-insert))
  "List of actions for `helm-fish-completion'."
  :group 'helm-fish-completion
  :type '(alist :key-type string :value-type function))

(defvar helm-fish-completion-source
  (helm-build-sync-source "Completion"
    :candidates #'helm-fish-completion-shell-complete
    :action helm-fish-completion-actions
    :keymap helm-fish-completion-map
    :fuzzy-match helm-eshell-fuzzy-match
    :persistent-action 'ignore))

;;;###autoload
(defun helm-fish-completion ()
  "Helm interface for fish completion.
This is mostly useful for `M-x shell'.
For Eshell, see `helm-fish-completion-make-eshell-source'."
  (interactive)
  (helm :sources 'helm-fish-completion-source
        :buffer "*helm-fish-completion*"))

;;;###autoload
(defun helm-fish-completion-make-eshell-source ()
  "Make and return Helm sources for Eshell.
This is a good candidate for `helm-esh-pcomplete-build-source-fn'.
For `M-x shell', use `helm-fish-completion' instead."
  (list
   'helm-fish-completion-source
   (helm-make-source "Eshell completions" 'helm-esh-source
     :fuzzy-match helm-eshell-fuzzy-match)))

(provide 'helm-fish-completion)
;;; helm-fish-completion.el ends here
