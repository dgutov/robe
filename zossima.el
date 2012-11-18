;;; zossima.el --- Ruby from Emacs

;; Copyright © 2012 Phil Hagelberg
;; Copyright © 2012 Dmitry Gutov

;; Author: Phil Hagelberg
;; URL: https://github.com/technomancy/zossima
;; Version: 0.2
;; Created: 2012-10-24
;; Keywords: ruby convenience rails
;; EmacsWiki: Zossima
;; Package-Requires: ((inf-ruby "2.2.3"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Jump to definition, driven by a live Ruby subprocess.

;; Explain to me again why doesn't this exist yet?
;; If you tell me "tags files" I'm going to kick you.

;;; Install

;; M-x package-install zossima (j/k not yet)

;;; Usage

;; (add-hook 'ruby-mode-hook 'zossima-mode)
;;
;;  - M-. to jump to a definition
;;  - M-, to jump back
;;  - C-c C-k to refresh Rails environment
;;
;; Before using `zossima-jump', call `run-ruby' or `rinari-console'.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'inf-ruby)
(require 'etags)
(require 'json)
(require 'url)
(require 'ido)
(require 'cl)

(defvar zossima-ruby-path
  (let ((current (or load-file-name (buffer-file-name))))
    (concat (file-name-directory current) "/zossima.rb"))
  "Path to file containing Ruby implementation of Zossima")

(defvar zossima-port 24959)

(defvar zossima-regex "^\\([A-Z][A-Za-z0-9:]+\\)\\([#\\.]\\)\\([a-z0-9_]+[?!=]?\\)$")

(defvar zossima-max-retries 4)

(defun zossima-start ()
  "Ensure remote process has Zossima started."
  (comint-send-string (inf-ruby-proc)
                      (format "load '%s' unless defined? Zossima\n"
                              zossima-ruby-path))
  (comint-send-string (inf-ruby-proc)
                      (format "Zossima.start(%s)\n" zossima-port)))

(defun zossima-request (endpoint &rest args)
  (let* ((url (format "http://127.0.0.1:%s/%s/%s" zossima-port endpoint
                      (mapconcat 'url-hexify-string args "/")))
         (response-buffer (zossima-retrieve url))
         (value (with-current-buffer response-buffer
                  (goto-char (point-min))
                  (search-forward "\n\n")
                  (let ((json-array-type 'list))
                    (json-read)))))
    (kill-buffer response-buffer)
    value))

(defun zossima-retrieve (url &optional retries)
  (with-current-buffer (url-retrieve-synchronously url)
    (unless (memq url-http-response-status '(200 500))
      (when (or (not retries) (plusp retries))
        (kill-buffer)
        (sleep-for 0.3)
        (set-buffer
         (zossima-retrieve url (1- (or retries zossima-max-retries))))))
    (current-buffer)))

(defun zossima-ask ()
  "Prompt for module, method, and jump to its definition."
  (interactive)
  (let* ((modules (zossima-request "modules"))
         (module (ido-completing-read "Module: " modules))
         (targets (zossima-request "targets" module))
         (_ (unless targets (error "No jumpable methods found")))
         (target (ido-completing-read "Method: " targets))
         (_ (string-match zossima-regex target))
         (module (match-string 1 target))
         (type (if (string= "#" (match-string 2 target)) "instance" "module"))
         (method (match-string 3 target)))
    (zossima-jump-to module type method)))

(defun zossima-jump (arg)
  "Jump to the method or class at point, prompt for module or file if necessary.
If invoked with a prefix or no symbol at point, delegate to `zossima-ask'."
  (interactive "P")
  (zossima-start)
  (let ((thing (thing-at-point 'symbol)) instance super)
    (cond
     ((or (not thing) arg)
      (zossima-ask))
     ((let (case-fold-search) (string-match "\\`[A-Z]" thing))
      (zossima-jump-to-class thing))
     (t
      (let* ((target (save-excursion
                       (and (progn (beginning-of-thing 'symbol)
                                   (= ?. (char-before)))
                            (progn (forward-char -2)
                                   (thing-at-point 'symbol)))))
             (_ (when (save-excursion (end-of-thing 'symbol) (looking-at "!"))
                  (setq thing (concat thing "!"))))
             (_ (unless target (let ((ctx (zossima-context)))
                                 (setq target (first ctx)
                                       instance (second ctx))
                                 (when (string= thing "super")
                                   (setq thing (third ctx)
                                         super "yes")))))
             (_ (when (and target (string= thing "new"))
                  (setq thing "initialize"
                        instance "yes")))
             (modules (zossima-request "method_targets"
                                       thing target instance super))
             (_ (unless modules (error "Method not found")))
             (target (if (= 1 (length modules))
                         (car modules)
                       (assoc (ido-completing-read "Module: " modules nil t)
                              modules))))
        (zossima-jump-to (first target) (second target) thing))))))

(defun zossima-jump-to-class (name)
  (let ((paths (zossima-request "class_locations" name)))
    (when (null paths) (error "Can't find the location"))
    (let ((file (if (= (length paths) 1)
                    (car paths)
                  (let ((alist (zossima-to-abbr-paths paths)))
                    (cdr (assoc (ido-completing-read "File: " alist nil t)
                                alist))))))
      (ring-insert find-tag-marker-ring (point-marker))
      (find-file file)
      (goto-char (point-min))
      (let* ((nesting (split-string name "::"))
             (cnt (1- (length nesting))))
        (re-search-forward (concat "^[ \t]*\\(class\\|module\\) +"
                                   (loop for i from 1 to cnt
                                         concat "\\(")
                                   (mapconcat #'identity nesting "\\)?")
                                   "\\_>")))
      (back-to-indentation))))

(defun zossima-to-abbr-paths (list)
  (let* ((sorted (sort (copy-sequence list) #'string-lessp))
         (first (first sorted))
         (last (car (last sorted)))
         (len (loop for i from 0 to (min (length first)
                                         (length last))
                    when (/= (aref first i) (aref last i))
                    return i)))
    (while (/= (aref first (1- len)) ?/) (decf len))
    (mapcar (lambda (path) (cons (substring path len) path)) list)))

(defun zossima-context ()
  (let* ((current-method (ruby-add-log-current-method))
         ;; Side-stepping the class methods bug in the above function.
         (segments (split-string current-method "#\\|\\.\\|::" t))
         (method-name (when (string-match "\\.\\|#" current-method)
                        (car (last segments))))
         (class (or (string-match "\\." current-method)
                    (not (string-match "#" current-method))))
         (target (mapconcat 'identity
                            (if method-name (butlast segments) segments) "::")))
    (set-text-properties 0 (length target) nil target) ;; for ease of debugging
    (set-text-properties 0 (length method-name) nil method-name)
    (list target (unless class "yes") method-name)))

(defun zossima-jump-to (module type method)
  (let ((location (zossima-request "location" module type method)))
    (if (null location)
        (message "Can't jump to a C method")
      (ring-insert find-tag-marker-ring (point-marker))
      (find-file (nth 0 location))
      (goto-char (point-min))
      (forward-line (1- (nth 1 location)))
      (back-to-indentation))))

(defun zossima-rails-refresh ()
  "Pick up changes in the loaded classes and detect new files.
Only works with Rails, see e.g. `rinari-console'."
  (interactive)
  (zossima-start)
  (zossima-request "rails_refresh")
  (message "Done"))

(defvar zossima-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map ruby-mode-map)
    (define-key map (kbd "M-.") 'zossima-jump)
    (define-key map (kbd "M-,") 'pop-tag-mark)
    (define-key map (kbd "C-c C-k") 'zossima-rails-refresh)
    map))

;;;###autoload
(define-minor-mode zossima-mode
  "Improved navigation for Ruby"
  nil " zossima" zossima-mode-map)

(provide 'zossima)
;;; zossima.el ends here
