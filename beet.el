;;; beet.el --- Code navigation, documentation and completion for Ruby

;; Copyright © 2012 Phil Hagelberg
;; Copyright © 2012 Dmitry Gutov

;; Author: Dmitry Gutov
;; URL: https://github.com/dgutov/beet
;; Version: 0.5
;; Keywords: ruby convenience rails
;; Package-Requires: ((inf-ruby "2.2.3"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; You can jump to or read the documentation for the method, module (jump only),
;; `super` or constructor definition at point.
;;
;; ElDoc support and constant and method completion are also provided.

;;; Usage

;; (add-hook 'ruby-mode-hook 'beet-mode)
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

(defvar beet-ruby-path
  (let ((current (or load-file-name (buffer-file-name))))
    (concat (file-name-directory current) "beet.rb"))
  "Path to the backend Ruby code.")

(defvar beet-port 24969)

(defvar beet-max-retries 4)

(defvar beet-jump-conservative nil)

(defvar beet-running nil)

(defun beet-start (&optional arg)
  "Start Beet server if it isn't already running."
  (interactive "p")
  (when (or arg (not beet-running))
    (comint-send-string (inf-ruby-proc)
                        (format "load '%s' unless defined? Beet\n"
                                beet-ruby-path))
    (comint-send-string (inf-ruby-proc)
                        (format "Beet.start(%s)\n" beet-port))
    (if (beet-request "ping")
        (setq beet-running t)
      (error "Server doesn't respond"))))

(defun beet-request (endpoint &rest args)
  (let* ((url (format "http://127.0.0.1:%s/%s/%s" beet-port endpoint
                      (mapconcat (lambda (arg)
                                   (cond ((eq arg t) "yes")
                                         ((plusp (length arg))
                                          (url-hexify-string arg))
                                         (t "_")))
                                 args "/")))
         (response-buffer (beet-retrieve url))
         (value (with-current-buffer response-buffer
                  (goto-char url-http-end-of-headers)
                  (let ((json-array-type 'list))
                    (json-read)))))
    (kill-buffer response-buffer)
    value))

(defun beet-retrieve (url &optional retries)
  (declare (special url-http-response-status))
  (with-current-buffer (url-retrieve-synchronously url)
    (unless (memq url-http-response-status '(200 500))
      (if (and retries (not (plusp retries)))
          (setq beet-running nil)
        (kill-buffer)
        (sleep-for 0.3)
        (set-buffer
         (beet-retrieve url (1- (or retries beet-max-retries))))))
    (current-buffer)))

(defun beet-ask ()
  "Prompt for module, method, and jump to its definition."
  (interactive)
  (beet-jump-to (beet-ask-prompt)))

(defun beet-ask-prompt ()
  (let* ((modules (beet-request "modules"))
         (module (ido-completing-read "Module: " modules))
         (targets (beet-request "targets" module))
         (_ (unless targets (error "No methods found")))
         (alist (beet-decorate-methods (cdr targets))))
    (cdr (assoc (ido-completing-read "Method: " alist nil t)
                alist))))

(defun beet-decorate-methods (list)
  (mapcar (lambda (row)
            (cons (concat (if (string= "instance" (second row)) "#" ".")
                          (third row))
                  row))
          list))

(defun beet-module-p (thing)
  (let (case-fold-search) (string-match "\\`\\([A-Z]\\|::\\)" thing)))

(defun beet-jump (arg)
  "Jump to the method or module at point, prompt for module or file if necessary.
If invoked with a prefix or no symbol at point, delegate to `beet-ask'."
  (interactive "P")
  (beet-start)
  (let ((thing (thing-at-point 'symbol)))
    (cond
     ((or (not thing) arg)
      (beet-ask))
     ((beet-module-p thing)
      (beet-jump-to-module thing))
     (t
      (beet-jump-to (beet-jump-prompt thing))))))

(defun beet-jump-prompt (thing)
  (let* ((alist (beet-decorate-modules (beet-jump-modules thing))))
    (unless alist (error "Method not found"))
    (if (= 1 (length alist))
        (cdar alist)
      (cdr (assoc (ido-completing-read "Module: " alist nil t)
                  alist)))))

(defun beet-jump-modules (thing)
  (destructuring-bind (target module instance ctx) (beet-call-context)
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
      (beet-request "method_targets"
                       thing target module instance super
                       beet-jump-conservative))))

(defun beet-call-context ()
  (let* ((target (save-excursion
                   (and (progn (beginning-of-thing 'symbol)
                               (= ?. (char-before)))
                        (progn (forward-char -1)
                               (thing-at-point 'symbol)))))
         (ctx (beet-context))
         (module (first ctx))
         (_ (when (string= target "self") (setq target nil)))
         (instance (unless target (second ctx))))
    (list target module instance ctx)))

(defun beet-decorate-modules (list)
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

(defun beet-jump-to-module (name)
  "Prompt for module, jump to a file where it has method definitions."
  (interactive `(,(ido-completing-read "Module: " (beet-request "modules"))))
  (let ((paths (beet-request "class_locations" name (car (beet-context)))))
    (when (null paths) (error "Can't find the location"))
    (let ((file (if (= (length paths) 1)
                    (car paths)
                  (let ((alist (beet-to-abbr-paths paths)))
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

(defun beet-to-abbr-paths (list)
  (let* ((sorted (sort (copy-sequence list) #'string-lessp))
         (first (first sorted))
         (last (car (last sorted)))
         (len (loop for i from 0 to (min (length first)
                                         (length last))
                    when (/= (aref first i) (aref last i))
                    return i)))
    (while (/= (aref first (1- len)) ?/) (decf len))
    (mapcar (lambda (path) (cons (substring path len) path)) list)))

(defun beet-context ()
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

(defun beet-jump-to (info)
  (let ((location (cdddr info)))
    (if (null location)
        (when (yes-or-no-p "Can't jump to a C method. Show documentation? ")
          (beet-show-doc info))
      (ring-insert find-tag-marker-ring (point-marker))
      (find-file (nth 0 location))
      (goto-char (point-min))
      (forward-line (1- (nth 1 location)))
      (back-to-indentation))))

(defun beet-rails-refresh ()
  "Pick up changes in the loaded classes and detect new files.
Only works with Rails, see e.g. `rinari-console'."
  (interactive)
  (beet-start)
  (beet-request "rails_refresh")
  (message "Done"))

(defun beet-doc (arg)
  "Show docstring for the method at point."
  (interactive "P")
  (beet-start)
  (let ((thing (thing-at-point 'symbol)))
    (beet-show-doc (if (or (not thing) arg)
                          (beet-ask-prompt)
                        (beet-jump-prompt thing)))))

(defvar beet-code-face 'font-lock-preprocessor-face)

(defvar beet-em-face 'font-lock-variable-name-face)

(defvar beet-doc-rules
  '(("<\\(tt\\|code\\)>\\([^<]+\\)</\\1>" 2 beet-code-face)
    ("\\_<\\+\\([^[:space:]]+\\)\\+\\_>" 1 beet-code-face)
    ("<\\(i\\|em\\)>\\([^<]+\\)</\\1>" 2 beet-em-face)
    ("\\_<_\\([^[:space:]]*\\)_\\_>" 1 beet-em-face)))

(defun beet-show-doc (info)
  (interactive)
  (let ((doc (beet-doc-for info))
        (buffer (get-buffer-create "*beet-doc*"))
        (inhibit-read-only t))
    (with-help-window buffer
      (princ (cdr (assoc 'signature doc)))
      (princ "\n\n")
      (princ (cdr (assoc 'comment doc))))
    (with-current-buffer buffer
      (beet-doc-apply-rules)
      (visual-line-mode 1))))

(defun beet-doc-apply-rules ()
  (loop for (re n sym) in beet-doc-rules do
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (replace-match (format "\\%d" n))
          (put-text-property (match-beginning 0) (match-end 0)
                             'face (symbol-value sym)))))

(defun beet-doc-for (info)
  (apply 'beet-request "doc_for" (subseq info 0 3)))

(defun beet-call-at-point ()
  (let ((state (syntax-ppss)) pt)
    (unless (nth 8 state)
      (when (and (not (ignore-errors (save-excursion
                                       (eq ?. (char-before
                                               (beginning-of-thing 'symbol))))))
                 (plusp (nth 0 state))
                 (eq (char-after (nth 1 state)) ?\())
        (goto-char (nth 1 state))
        (skip-chars-backward " "))
      (let ((thing (thing-at-point 'symbol)))
        (when (and thing
                   (or (string= thing "super")
                       (not (memq (get-text-property 0 'face thing)
                                  '(font-lock-function-name-face
                                    font-lock-keyword-face)))))
          thing)))))

(defun beet-eldoc ()
  (save-excursion
    (let ((thing (beet-call-at-point))
          (url-show-status nil)
          (beet-max-retries 0))
      (when (and thing beet-running (not (beet-module-p thing)))
        (let* ((beet-jump-conservative t)
               (list (loop for info in (beet-jump-modules thing)
                           when (car info) collect info)))
          (when (consp list)
            (let* ((doc (beet-doc-for (car list)))
                   (summary (with-temp-buffer
                              (insert (cdr (assoc 'comment doc)))
                              (unless (= (point) (point-min))
                                (goto-char (point-min))
                                (save-excursion
                                  (forward-sentence)
                                  (delete-region (point) (point-max))
                                  (beet-doc-apply-rules))
                                (while (search-forward "\n" nil t)
                                  (replace-match " ")))
                              (buffer-string)))
                   (msg (format "%s %s" (cdr (assoc 'signature doc)) summary)))
              (substring msg 0 (min (frame-width) (length msg))))))))))

(defun beet-complete-at-point ()
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (list (car bounds) (cdr bounds)
            (completion-table-dynamic #'beet-complete-thing)))))

(defun beet-complete-thing (thing)
  (setq this-command 'beet-complete-thing)
  (beet-start)
  (if (beet-module-p thing)
      (beet-request "complete_const" thing)
    (destructuring-bind (target module instance _) (beet-call-context)
      (beet-request "complete_method" thing target module instance))))

(defvar beet-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") 'beet-jump)
    (define-key map (kbd "M-,") 'pop-tag-mark)
    (define-key map (kbd "C-c C-d") 'beet-doc)
    (define-key map (kbd "C-c C-k") 'beet-rails-refresh)
    map))

;;;###autoload
(define-minor-mode beet-mode
  "Improved navigation for Ruby"
  nil " beet" beet-mode-map
  (add-to-list 'completion-at-point-functions 'beet-complete-at-point)
  (set (make-local-variable 'eldoc-documentation-function) 'beet-eldoc)
  (eldoc-add-command 'beet-complete-thing)
  (turn-on-eldoc-mode))

(provide 'beet)
;;; beet.el ends here
