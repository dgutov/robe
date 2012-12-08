;;; robe.el --- Code navigation, documentation and completion for Ruby

;; Copyright © 2012 Phil Hagelberg
;; Copyright © 2012 Dmitry Gutov

;; Author: Dmitry Gutov
;; URL: https://github.com/dgutov/robe
;; Version: 0.6
;; Keywords: ruby convenience rails
;; Package-Requires: ((inf-ruby "2.2.3"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; You can jump to or read the documentation for the method, module (jump only),
;; `super` or constructor definition at point.
;;
;; ElDoc support and constant and method completion are also provided.

;;; Usage

;; (add-hook 'ruby-mode-hook 'robe-mode)
;;
;;  - M-. to jump to the definition
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
(require 'ruby-mode)

(defvar robe-ruby-path
  (let ((current (or load-file-name (buffer-file-name))))
    (concat (file-name-directory current) "lib"))
  "Path to the backend Ruby code.")

(defvar robe-port 24969)

(defvar robe-max-retries 4)

(defvar robe-jump-conservative nil)

(defvar robe-running nil)

(defun robe-start (&optional arg)
  "Start Robe server if it isn't already running."
  (interactive "p")
  (when (or arg (not robe-running))
    (let ((script (format (mapconcat #'identity
                                     '("unless defined? Robe"
                                       "  $:.unshift '%s'"
                                       "  require 'robe'"
                                       "end"
                                       "Robe.start(%d)\n")
                                     ";")
                          robe-ruby-path robe-port)))
      (comint-send-string (inf-ruby-proc) script))
    (if (robe-request "ping")
        (setq robe-running t)
      (error "Server doesn't respond"))))

(defun robe-request (endpoint &rest args)
  (let* ((url (format "http://127.0.0.1:%s/%s/%s" robe-port endpoint
                      (mapconcat (lambda (arg)
                                   (cond ((eq arg t) "yes")
                                         ((plusp (length arg))
                                          (url-hexify-string arg))
                                         (t "-")))
                                 args "/")))
         (response-buffer (robe-retrieve url))
         (value (with-current-buffer response-buffer
                  (goto-char url-http-end-of-headers)
                  (let ((json-array-type 'list))
                    (json-read)))))
    (kill-buffer response-buffer)
    value))

(defun robe-retrieve (url &optional retries)
  (declare (special url-http-response-status))
  (with-current-buffer (url-retrieve-synchronously url)
    (unless (memq url-http-response-status '(200 500))
      (if (and retries (not (plusp retries)))
          (setq robe-running nil)
        (kill-buffer)
        (sleep-for 0.3)
        (set-buffer
         (robe-retrieve url (1- (or retries robe-max-retries))))))
    (current-buffer)))

(defun robe-ask ()
  "Prompt for module, method, and jump to its definition."
  (interactive)
  (robe-jump-to (robe-ask-prompt)))

(defun robe-ask-prompt ()
  (let* ((modules (robe-request "modules"))
         (module (ido-completing-read "Module: " modules))
         (targets (robe-request "targets" module))
         (_ (unless targets (error "No methods found")))
         (alist (robe-decorate-methods (cdr targets))))
    (cdr (assoc (ido-completing-read "Method: " alist nil t)
                alist))))

(defun robe-decorate-methods (list)
  (mapcar (lambda (row)
            (cons (concat (if (string= "instance" (second row)) "#" ".")
                          (third row))
                  row))
          list))

(defun robe-module-p (thing)
  (let (case-fold-search) (string-match "\\`\\([A-Z]\\|::\\)" thing)))

(defun robe-jump (arg)
  "Jump to the method or module at point, prompt for module or file if necessary.
If invoked with a prefix or no symbol at point, delegate to `robe-ask'."
  (interactive "P")
  (robe-start)
  (let ((thing (thing-at-point 'symbol)))
    (cond
     ((or (not thing) arg)
      (robe-ask))
     ((robe-module-p thing)
      (robe-jump-to-module thing))
     (t
      (robe-jump-to (robe-jump-prompt thing))))))

(defun robe-jump-prompt (thing)
  (let* ((alist (robe-decorate-modules (robe-jump-modules thing))))
    (unless alist (error "Method not found"))
    (if (= 1 (length alist))
        (cdar alist)
      (cdr (assoc (ido-completing-read "Module: " alist nil t)
                  alist)))))

(defun robe-jump-modules (thing)
  (destructuring-bind (target module instance ctx) (robe-call-context)
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
      (robe-request "method_targets"
                       thing target module instance super
                       robe-jump-conservative))))

(defun robe-call-context ()
  (let* ((target (save-excursion
                   (and (progn (ignore-errors (beginning-of-thing 'symbol))
                               (= ?. (char-before)))
                        (progn (forward-char -1)
                               (thing-at-point 'symbol)))))
         (ctx (robe-context))
         (module (first ctx))
         (_ (when (string= target "self") (setq target nil)))
         (instance (unless target (second ctx))))
    (list target module instance ctx)))

(defun robe-decorate-modules (list)
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

(defun robe-jump-to-module (name)
  "Prompt for module, jump to a file where it has method definitions."
  (interactive `(,(ido-completing-read "Module: " (robe-request "modules"))))
  (let ((paths (robe-request "class_locations" name (car (robe-context)))))
    (when (null paths) (error "Can't find the location"))
    (let ((file (if (= (length paths) 1)
                    (car paths)
                  (let ((alist (robe-to-abbr-paths paths)))
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

(defun robe-to-abbr-paths (list)
  (let* ((sorted (sort (copy-sequence list) #'string-lessp))
         (first (first sorted))
         (last (car (last sorted)))
         (len (loop for i from 0 to (min (length first)
                                         (length last))
                    when (/= (aref first i) (aref last i))
                    return i)))
    (unless (zerop len)
      (while (/= (aref first (1- len)) ?/) (decf len)))
    (mapcar (lambda (path) (cons (substring path len) path)) list)))

(defun robe-context ()
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

(defun robe-jump-to (info)
  (let ((location (cdddr info)))
    (if (null location)
        (when (yes-or-no-p "Can't jump to a C method. Show documentation? ")
          (robe-show-doc info))
      (ring-insert find-tag-marker-ring (point-marker))
      (find-file (nth 0 location))
      (goto-char (point-min))
      (forward-line (1- (nth 1 location)))
      (back-to-indentation))))

(defun robe-rails-refresh ()
  "Pick up changes in the loaded classes and detect new files.
Only works with Rails, see e.g. `rinari-console'."
  (interactive)
  (robe-start)
  (robe-request "rails_refresh")
  (message "Done"))

(defun robe-doc (arg)
  "Show docstring for the method at point."
  (interactive "P")
  (robe-start)
  (let ((thing (thing-at-point 'symbol)))
    (robe-show-doc (if (or (not thing) arg)
                          (robe-ask-prompt)
                        (robe-jump-prompt thing)))))

(defvar robe-code-face 'font-lock-preprocessor-face)

(defvar robe-em-face 'font-lock-variable-name-face)

(defvar robe-doc-rules
  '(("<\\(tt\\|code\\)>\\(.+?\\)</\\1>" robe-doc-hl-text 2 robe-code-face)
    ("\\_<\\+\\([^[:space:]]+\\)\\+\\_>" robe-doc-hl-text 1 robe-code-face)
    ("<\\(i\\|em\\)>\\(.+?\\)</\\1>" robe-doc-hl-text 2 robe-em-face)
    ("\\_<_\\([^_][^[:space:]]*\\)_\\_>" robe-doc-hl-text 1 robe-em-face)
    ("\\(``\\).*?\\(''\\)" robe-doc-replace-text (1 . "\u201c") (2 . "\u201d"))
    ("\\(`\\).*?\\('\\)" robe-doc-replace-text (1 . "\u2018") (2 . "\u2019"))))

(defun robe-show-doc (info)
  (interactive)
  (let* ((doc (robe-doc-for info))
         (buffer (get-buffer-create "*robe-doc*"))
         (inhibit-read-only t)
         (docstring (cdr (assoc 'docstring doc))))
    (with-help-window buffer
      (unless (zerop (length docstring))
        (princ "\n\n")
        (princ docstring)))
    (with-current-buffer buffer
      (robe-doc-fontify-regions)
      (goto-char (point-min))
      (save-excursion
        (insert (robe-signature info (cdr (assoc 'parameters doc)))))
      (visual-line-mode 1))))

(defun robe-doc-fontify-regions ()
  (let (last-pos)
    (while (not (eobp))
      (when last-pos (robe-doc-apply-rules last-pos (point)))
      (while (looking-at "\\(\n\\)+\\( +.*\n\\)+\\(\n\\|\\'\\)")
        (robe-doc-fontify-code (match-beginning 0) (match-end 0))
        (goto-char (match-end 2)))
      (setq last-pos (point))
      (re-search-forward "[^[:space:]]\n *$" nil 'move))
    (robe-doc-apply-rules last-pos (point))))

(defun robe-doc-apply-rules (from to)
  (let ((table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?- "." table)
    (with-syntax-table table
      (save-excursion
        (goto-char from)
        (loop for (re fn . args) in robe-doc-rules do
              (save-excursion
                (while (re-search-forward re to t)
                  (apply fn args))))))))

(defun robe-doc-hl-text (group face)
  (replace-match (format "\\%d" group))
  (put-text-property (match-beginning 0) (match-end 0)
                     'face (symbol-value face)))

(defun robe-doc-replace-text (&rest rules)
  (loop for (group . replacement) in rules do
        (replace-match replacement t nil nil group)))

(defun robe-doc-fontify-code (from to)
  (let ((syntax-propertize-function #'ruby-syntax-propertize-function)
        (font-lock-defaults (list ruby-font-lock-keywords nil nil))
        (font-lock-syntax-table ruby-font-lock-syntax-table)
        (font-lock-dont-widen t))
    (save-restriction
      (narrow-to-region from to)
      (font-lock-fontify-region from to))))

(defun robe-signature (info params &optional arg-num)
  (destructuring-bind (mod instance method &rest) info
    (let ((cnt 0) args)
      (dolist (pair params)
        (let ((kind (intern (first pair)))
              (name (second pair)))
          (incf cnt)
          (unless name
            (setq name
                  (case kind
                    (rest "args")
                    (block "block")
                    (t (format "arg%s" cnt)))))
          (push (propertize (format (case kind
                                      (rest "%s...")
                                      (block "&%s")
                                      (opt "[%s]")
                                      (t "%s")) name)
                            'face (if (and arg-num
                                       (or (= arg-num cnt)
                                           (and (eq kind 'rest)
                                                (> arg-num cnt))))
                                      (list robe-em-face 'bold)
                                    robe-em-face))
                args)))
      (concat (mapconcat (lambda (s) (propertize s 'face font-lock-type-face))
                         (split-string mod "::" t) "::")
              (if instance "#" ".")
              (propertize method 'face font-lock-function-name-face)
              "(" (mapconcat #'identity (nreverse args) ", ") ")"))))

(defun robe-doc-for (info)
  (apply 'robe-request "doc_for" (subseq info 0 3)))

(defun robe-call-at-point ()
  (let ((state (syntax-ppss)) (start (point))
        in-arglist)
    (unless (nth 8 state)
      (when (and (not (ignore-errors (save-excursion
                                       (eq ?. (char-before
                                               (beginning-of-thing 'symbol))))))
                 (plusp (nth 0 state))
                 (eq (char-after (nth 1 state)) ?\())
        (setq in-arglist t)
        (goto-char (nth 1 state))
        (skip-chars-backward " "))
      (let ((thing (thing-at-point 'symbol)))
        (when (and thing
                   (or (string= thing "super")
                       (not (memq (get-text-property 0 'face thing)
                                  (list font-lock-function-name-face
                                        font-lock-keyword-face)))))
          (cons thing (when in-arglist
                        (robe-call-arg-num (point) start))))))))

(defun robe-call-arg-num (from point)
  (save-excursion
    (let ((depth (car (save-excursion (parse-partial-sexp from point))))
          (n 1))
      (while (re-search-forward "," point t)
        (let ((state (parse-partial-sexp from (point))))
          (when (and (= depth (car state)) (not (nth 8 state)))
            (incf n))))
      n)))

(defun robe-eldoc ()
  (save-excursion
    (let* ((call (robe-call-at-point))
           (thing (car call))
           (arg-num (cdr call))
           (url-show-status nil)
           (robe-max-retries 0))
      (when (and thing robe-running (not (robe-module-p thing)))
        (let* ((robe-jump-conservative t)
               (list (loop for info in (robe-jump-modules thing)
                           when (car info) collect info)))
          (when (consp list)
            (let* ((info (car list))
                   (doc (robe-doc-for info))
                   (summary (with-temp-buffer
                              (insert (cdr (assoc 'docstring doc)))
                              (unless (= (point) (point-min))
                                (goto-char (point-min))
                                (save-excursion
                                  (forward-sentence)
                                  (delete-region (point) (point-max)))
                                (robe-doc-apply-rules (point) (point-max))
                                (while (search-forward "\n" nil t)
                                  (replace-match " ")))
                              (buffer-string)))
                   (sig (robe-signature info (cdr (assoc 'parameters doc))
                                        arg-num))
                   (msg (format "%s %s" sig summary)))
              (substring msg 0 (min (frame-width) (length msg))))))))))

(defun robe-complete-at-point ()
  (let ((bounds (bounds-of-thing-at-point 'symbol))
        (fn (completion-table-dynamic #'robe-complete-thing)))
    (if bounds
        (list (car bounds) (cdr bounds) fn)
      (list (point) (point) fn))))

(defun robe-complete-thing (thing)
  (setq this-command 'robe-complete-thing)
  (robe-start)
  (if (robe-module-p thing)
      (robe-request "complete_const" thing)
    (destructuring-bind (target module instance _) (robe-call-context)
      (robe-request "complete_method" thing target module instance))))

(defvar robe-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") 'robe-jump)
    (define-key map (kbd "M-,") 'pop-tag-mark)
    (define-key map (kbd "C-c C-d") 'robe-doc)
    (define-key map (kbd "C-c C-k") 'robe-rails-refresh)
    map))

;;;###autoload
(define-minor-mode robe-mode
  "Improved navigation for Ruby"
  nil " robe" robe-mode-map
  (add-to-list 'completion-at-point-functions 'robe-complete-at-point)
  (set (make-local-variable 'eldoc-documentation-function) 'robe-eldoc)
  (eldoc-add-command 'robe-complete-thing)
  (turn-on-eldoc-mode))

(provide 'robe)
;;; robe.el ends here
