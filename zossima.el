;;; zossima.el --- Ruby from Emacs

;; Copyright © 2012 Phil Hagelberg
;; Copyright © 2012 Dmitry Gutov

;; Author: Phil Hagelberg
;; URL: https://github.com/technomancy/zossima
;; Version: 0.4
;; Created: 2012-10-24
;; Keywords: ruby convenience rails
;; EmacsWiki: Zossima
;; Package-Requires: ((inf-ruby "2.2.3"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Jump to definition, driven by a live Ruby subprocess.

;;; Install

;; See the README.

;;; Usage

;; (add-hook 'ruby-mode-hook 'zossima-mode)
;;
;;  - M-. to jump to a definition
;;  - M-, to jump back
;;  - C-c C-d to see the documentation
;;  - C-c C-k to refresh Rails environment
;;  - C-M-i to complete the symbol at point
;;
;; Before using any of these commands, call `run-ruby' or `rinari-console'.

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

(defvar zossima-jump-conservative nil)

(defvar zossima-running nil)

(defun zossima-start ()
  "Ensure remote process has Zossima started."
  (comint-send-string (inf-ruby-proc)
                      (format "load '%s' unless defined? Zossima\n"
                              zossima-ruby-path))
  (comint-send-string (inf-ruby-proc)
                      (format "Zossima.start(%s)\n" zossima-port))
  (setq zossima-running t))

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
      (if (and retries (not (plusp retries)))
          (setq zossima-running nil)
        (kill-buffer)
        (sleep-for 0.3)
        (set-buffer
         (zossima-retrieve url (1- (or retries zossima-max-retries))))))
    (current-buffer)))

(defun zossima-ask ()
  "Prompt for module, method, and jump to its definition."
  (interactive)
  (zossima-jump-to (zossima-ask-prompt)))

(defun zossima-ask-prompt ()
  (let* ((modules (zossima-request "modules"))
         (module (ido-completing-read "Module: " modules))
         (targets (zossima-request "targets" module))
         (_ (unless targets (error "No methods found")))
         (alist (zossima-decorate-methods (cdr targets))))
    (cdr (assoc (ido-completing-read "Method: " alist nil t)
                alist))))

(defun zossima-decorate-methods (list)
  (mapcar (lambda (row)
            (cons (concat (if (string= "instance" (second row)) "#" ".")
                          (third row))
                  row))
          list))

(defun zossima-module-p (thing)
  (let (case-fold-search) (string-match "\\`\\([A-Z]\\|::\\)" thing)))

(defun zossima-jump (arg)
  "Jump to the method or module at point, prompt for module or file if necessary.
If invoked with a prefix or no symbol at point, delegate to `zossima-ask'."
  (interactive "P")
  (zossima-start)
  (let ((thing (thing-at-point 'symbol)))
    (cond
     ((or (not thing) arg)
      (zossima-ask))
     ((zossima-module-p thing)
      (zossima-jump-to-module thing))
     (t
      (zossima-jump-to (zossima-jump-prompt thing))))))

(defun zossima-jump-prompt (thing)
  (let* ((modules (zossima-jump-modules thing)))
    (unless modules (error "Method not found"))
    (if (= 1 (length modules))
        (car modules)
      (let ((alist (zossima-decorate-modules modules)))
        (cdr (assoc (ido-completing-read "Module: " alist nil t)
                    alist))))))

(defun zossima-jump-modules (thing)
  (destructuring-bind (target module instance ctx) (zossima-call-context)
    (let (super)
      (when (save-excursion (end-of-thing 'symbol) (looking-at "!"))
        (setq thing (concat thing "!")))
      (unless target
        (when (string= thing "super")
          (setq thing (third ctx)
                super t)))
      (when (and target (string= thing "new"))
        (setq thing "initialize"
              instance t))
      (when (and target (save-excursion
                          (end-of-thing 'symbol)
                          (looking-at " *=[^=]")))
        (setq thing (concat thing "=")))
      (zossima-request "method_targets"
                       thing target module instance super
                       zossima-jump-conservative))))

(defun zossima-call-context ()
  (let* ((target (save-excursion
                   (and (progn (beginning-of-thing 'symbol)
                               (= ?. (char-before)))
                        (progn (forward-char -1)
                               (thing-at-point 'symbol)))))
         (ctx (zossima-context))
         (module (first ctx))
         (_ (when (string= target "self") (setq target nil)))
         (instance (unless target (second ctx))))
    (list target module instance ctx)))

(defun zossima-decorate-modules (list)
  (loop for row in list
        for name = (cond ((first row) (first row))
                         ((nth 3 row)
                          (format "<%s>" (file-name-nondirectory (nth 3 row)))))
        when name
        collect (cons (concat name
                              (if (string= "instance"
                                           (second row))
                                  "#" "."))
                      (cons name (cdr row)))))

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
        (when (yes-or-no-p "Can't jump to a C method. Show documentation? ")
          (zossima-show-doc info))
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

(defun zossima-doc (arg)
  "Show docstring for the method at point."
  (interactive "P")
  (zossima-start)
  (let ((thing (thing-at-point 'symbol)))
    (zossima-show-doc (if (or (not thing) arg)
                          (zossima-ask-prompt)
                        (zossima-jump-prompt thing)))))

(defvar zossima-code-face 'font-lock-preprocessor-face)

(defvar zossima-em-face 'font-lock-variable-name-face)

(defvar zossima-doc-rules
  '(("<\\(tt\\|code\\)>\\([^<]+\\)</\\1>" 2 zossima-code-face)
    ("\\_<\\+\\([^[:space:]]+\\)\\+\\_>" 1 zossima-code-face)
    ("<\\(i\\|em\\)>\\([^<]+\\)</\\1>" 2 zossima-em-face)
    ("\\_<_\\([^[:space:]]*\\)_\\_>" 1 zossima-em-face)))

(defun zossima-show-doc (info)
  (interactive)
  (let ((doc (zossima-doc-for info))
        (buffer (get-buffer-create "*zossima-doc*"))
        (inhibit-read-only t))
    (with-help-window buffer
      (princ (cdr (assoc 'signature doc)))
      (princ "\n\n")
      (princ (cdr (assoc 'comment doc))))
    (with-current-buffer buffer
      (zossima-doc-apply-rules)
      (visual-line-mode 1))))

(defun zossima-doc-apply-rules ()
  (loop for (re n sym) in zossima-doc-rules do
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (replace-match (format "\\%d" n))
          (put-text-property (match-beginning 0) (match-end 0)
                             'face (symbol-value sym)))))

(defun zossima-doc-for (info)
  (apply 'zossima-request "doc_for" (subseq info 0 3)))

(defun zossima-call-at-point ()
  (let ((state (syntax-ppss)) pt)
    (unless (nth 8 state)
      (when (and (not (ignore-errors (save-excursion
                                       (eq ?. (char-before
                                               (beginning-of-thing 'symbol))))))
                 (plusp (nth 0 state))
                 (eq (char-after (nth 1 state)) ?\())
        (goto-char (nth 1 state))
        (skip-chars-backward " "))
      (let* ((bounds (bounds-of-thing-at-point 'symbol))
             (beg (car bounds))
             (end (cdr bounds)))
        (unless (or (not bounds)
                    (text-property-any beg end 'face font-lock-function-name-face)
                    (text-property-any beg end 'face font-lock-keyword-face))
          (buffer-substring beg end))))))

(defun zossima-eldoc ()
  (save-excursion
    (let ((thing (zossima-call-at-point))
          (url-show-status nil)
          (zossima-max-retries 0))
      (when (and thing zossima-running (not (zossima-module-p thing)))
        (let* ((zossima-jump-conservative t)
               (list (loop for info in (zossima-jump-modules thing)
                           when (car info) collect info)))
          (when (consp list)
            (let* ((doc (zossima-doc-for (car list)))
                   (summary (with-temp-buffer
                              (insert (cdr (assoc 'comment doc)))
                              (unless (= (point) (point-min))
                                (goto-char (point-min))
                                (save-excursion
                                  (forward-sentence)
                                  (delete-region (point) (point-max))
                                  (zossima-doc-apply-rules))
                                (while (search-forward "\n" nil t)
                                  (replace-match " ")))
                              (buffer-string)))
                   (msg (format "%s %s" (cdr (assoc 'signature doc)) summary)))
              (substring msg 0 (min (frame-width) (length msg))))))))))

(defun zossima-complete-at-point ()
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (list (car bounds) (cdr bounds)
            (completion-table-dynamic #'zossima-complete-thing)))))

(defun zossima-complete-thing (thing)
  (setq this-command 'zossima-complete-thing)
  (zossima-start)
  (if (zossima-module-p thing)
      (zossima-request "complete_const" thing)
    (destructuring-bind (target module instance _) (zossima-call-context)
      (zossima-request "complete_method" thing target module instance))))

(defvar zossima-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") 'zossima-jump)
    (define-key map (kbd "M-,") 'pop-tag-mark)
    (define-key map (kbd "C-c C-d") 'zossima-doc)
    (define-key map (kbd "C-c C-k") 'zossima-rails-refresh)
    map))

;;;###autoload
(define-minor-mode zossima-mode
  "Improved navigation for Ruby"
  nil " zossima" zossima-mode-map
  (add-to-list 'completion-at-point-functions 'zossima-complete-at-point)
  (set (make-local-variable 'eldoc-documentation-function) 'zossima-eldoc)
  (eldoc-add-command 'zossima-complete-thing)
  (turn-on-eldoc-mode))

(provide 'zossima)
;;; zossima.el ends here
