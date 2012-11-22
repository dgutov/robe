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
(require 'url-http)
(require 'ido)
(require 'cl)

(defvar zossima-ruby-path
  (let ((current (or load-file-name (buffer-file-name))))
    (concat (file-name-directory current) "zossima.rb"))
  "Path to file containing Ruby implementation of Zossima")

(defvar zossima-port 24959)

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
                      (mapconcat (lambda (arg)
                                   (cond ((eq arg t) "yes")
                                         ((plusp (length arg))
                                          (url-hexify-string arg))
                                         (t "_")))
                                 args "/")))
         (response-buffer (zossima-retrieve url))
         (value (with-current-buffer response-buffer
                  (goto-char (point-min))
                  (search-forward "\n\n")
                  (let ((json-array-type 'list))
                    (json-read)))))
    (kill-buffer response-buffer)
    value))

(defun zossima-retrieve (url &optional retries)
  (declare (special url-http-response-status))
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
         (alist (zossima-decorate-methods (cdr targets))))
    (zossima-jump-to (cdr (assoc (ido-completing-read "Method: " alist nil t)
                                 alist)))))

(defun zossima-decorate-methods (list)
  (mapcar (lambda (row)
            (cons (concat (if (string= "instance" (second row)) "#" ".")
                          (third row))
                  row))
          list))

(defun zossima-jump (arg)
  "Jump to the method or module at point, prompt for module or file if necessary.
If invoked with a prefix or no symbol at point, delegate to `zossima-ask'."
  (interactive "P")
  (zossima-start)
  (let ((thing (thing-at-point 'symbol)) instance super module)
    (cond
     ((or (not thing) arg)
      (zossima-ask))
     ((let (case-fold-search) (string-match "\\`\\([A-Z]\\|::\\)" thing))
      (zossima-jump-to-module thing))
     (t
      (let* ((target (save-excursion
                       (and (progn (beginning-of-thing 'symbol)
                                   (= ?. (char-before)))
                            (progn (forward-char -2)
                                   (thing-at-point 'symbol)))))
             (_ (when (save-excursion (end-of-thing 'symbol) (looking-at "!"))
                  (setq thing (concat thing "!"))))
             (ctx (zossima-context))
             (module (first ctx))
             (_ (unless target
                  (setq instance (second ctx))
                  (when (string= thing "super")
                    (setq thing (third ctx)
                          super t))))
             (_ (when (and target (string= thing "new"))
                  (setq thing "initialize"
                        instance t)))
             (modules (zossima-request "method_targets"
                                       thing target module instance super))
             (_ (unless modules (error "Method not found"))))
        (zossima-jump-to (if (= 1 (length modules))
                             (car modules)
                           (let ((alist (zossima-decorate-modules modules)))
                             (cdr (assoc (ido-completing-read "Module: " alist nil t)
                                         alist))))))))))

(defun zossima-decorate-modules (list)
  (mapcar (lambda (row)
            (cons (concat (first row)
                          (if (string= "instance"
                                       (second row))
                              "#" "."))
                  row))
          list))

(defun zossima-jump-to-module (name)
  "Prompt for module, jump to a file where it has method definitions."
  (interactive `(,(ido-completing-read "Module: " (zossima-request "modules"))))
  (let ((paths (zossima-request "class_locations" name (car (zossima-context)))))
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
        (re-search-forward (concat "^[ \t]*\\(class\\|module\\) +.*\\_<"
                                   (loop for i from 1 to cnt
                                         concat "\\(")
                                   (mapconcat #'identity nesting "::\\)?")
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
  (let ((current-method (ruby-add-log-current-method)))
    (if current-method
        ;; Side-stepping the module methods bug in the above function.
        (let* ((segments (split-string current-method "#\\|\\.\\|::" t))
               (method-name (when (string-match "\\.\\|#" current-method)
                              (car (last segments))))
               (instance (string-match "#" current-method))
               (module (mapconcat 'identity (if method-name
                                                (butlast segments)
                                              segments) "::")))
          (set-text-properties 0 (length module) nil module) ;; for debugging
          (set-text-properties 0 (length method-name) nil method-name)
          (list module (when instance t) method-name))
      (list nil t nil))))

(defun zossima-jump-to (info)
  (let ((location (cdddr info)))
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
