;;; robe.el --- Code navigation, documentation lookup and completion for Ruby

;; Copyright © 2012 Phil Hagelberg
;; Copyright © 2012, 2013 Dmitry Gutov

;; Author: Dmitry Gutov
;; URL: https://github.com/dgutov/robe
;; Version: 0.7.1
;; Keywords: ruby convenience rails
;; Package-Requires: ((inf-ruby "2.2.4"))

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
(require 'thingatpt)
(require 'eldoc)
(require 'help-mode)
(require 'ruby-mode)

(defgroup robe nil
  "Code navigation, documentation lookup and completion for Ruby"
  :group 'languages
  :group 'convenience)

(defcustom robe-highlight-capf-candidates (version< "24.3.50" emacs-version)
  "When non-nil, `completion-at-point' candidates buffer will
have constants, methods and arguments highlighted in color."
  :group 'robe)

(defvar robe-ruby-path
  (let ((current (or load-file-name (buffer-file-name))))
    (expand-file-name "lib" (file-name-directory current)))
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
    (when (robe-request "ping") ;; Should be always t when no error, though.
      (setq robe-running t))))

(defun robe-request (endpoint &rest args)
  (declare (special url-http-end-of-headers))
  (let* ((url (format "http://127.0.0.1:%s/%s/%s" robe-port endpoint
                      (mapconcat (lambda (arg)
                                   (cond ((eq arg t) "yes")
                                         ((plusp (length arg))
                                          (url-hexify-string arg))
                                         (t "-")))
                                 args "/")))
         (response-buffer (robe-retrieve url)))
    (if response-buffer
        (prog1
            (with-current-buffer response-buffer
              (goto-char url-http-end-of-headers)
              (let ((json-array-type 'list))
                (json-read)))
          (kill-buffer response-buffer))
      (error "Server doesn't respond"))))

(defun robe-retrieve (url &optional retries)
  (declare (special url-http-response-status))
  (let ((buffer (condition-case nil (url-retrieve-synchronously url)
                  (file-error nil))))
    (if (and buffer
             (with-current-buffer buffer
               (memq url-http-response-status '(200 500))))
        buffer
      (if (and retries (not (plusp retries)))
          (setq robe-running nil)
        (when buffer
          (kill-buffer buffer))
        (sleep-for 0.3)
        (robe-retrieve url (1- (or retries robe-max-retries)))))))

(defstruct (robe-spec (:type list)) module inst-p method params file line)

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
  (mapcar (lambda (spec)
            (cons (concat (if (robe-spec-inst-p spec) "#" ".")
                          (robe-spec-method spec))
                  spec))
          list))

(defun robe-const-p (thing)
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
     ((robe-const-p thing)
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
  (loop for spec in list
        for name = (cond ((robe-spec-module spec))
                         ((robe-spec-file spec)
                          (format "<%s>" (file-name-nondirectory
                                          (robe-spec-file spec)))))
        when name
        collect (cons (concat name
                              (if (robe-spec-inst-p spec) "#" "."))
                      (cons name (cdr spec)))))

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
      (robe-find-file file)
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

(defun robe-jump-to (spec &optional pop-to-buffer)
  (let ((file (robe-spec-file spec)))
    (if (null file)
        (when (yes-or-no-p "Can't jump to a C method. Show documentation? ")
          (robe-show-doc spec))
      (robe-find-file file pop-to-buffer)
      (goto-char (point-min))
      (forward-line (1- (robe-spec-line spec)))
      (back-to-indentation))))

(defun robe-find-file (file &optional pop-to-buffer)
  (unless (file-exists-p file)
    (error "'%s' does not exist" file))
  (if pop-to-buffer
      (pop-to-buffer (find-file-noselect file))
    (ring-insert find-tag-marker-ring (point-marker))
    (find-file file)))

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

(define-button-type 'robe-method-def
  :supertype 'help-xref
  'help-function #'robe-jump-to
  'help-echo "mouse-2, RET: find method's definition")

(define-button-type 'robe-toggle-source
  'action 'robe-toggle-source
  'help-echo "mouse-2, RET: toggle source")

(defun robe-show-doc (spec)
  (interactive)
  (let* ((doc (robe-doc-for spec))
         (buffer (get-buffer-create "*robe-doc*"))
         (inhibit-read-only t)
         (docstring (cdr (assoc 'docstring doc)))
         (source (cdr (assoc 'source doc)))
         (aliases (cdr (assoc 'aliases doc)))
         (visibility (cdr (assoc 'visibility doc)))
         (file (robe-spec-file spec)))
    (with-help-window buffer
      (unless (zerop (length docstring))
        (princ "\n\n")
        (princ docstring)))
    (with-current-buffer buffer
      (robe-doc-fontify-regions)
      (when source
        (insert "\n")
        (let ((button (insert-text-button "Source"
                                          'type 'robe-toggle-source)))
          (insert "\n\n")
          (if file
              (let ((beg (point)))
                (insert source)
                (robe-doc-fontify-ruby beg (point)))
            (insert (robe-doc-fontify-c-string source)))
          (robe-toggle-source button)))
      (goto-char (point-min))
      (save-excursion
        (insert (robe-signature spec))
        (when file
          (insert " is defined in ")
          (insert-text-button (file-name-nondirectory file)
                              'type 'robe-method-def
                              'help-args (list spec t)))
        (when (equal visibility "public")
          (setq visibility nil))
        (when (or aliases visibility)
          (insert "\n"))
        (when aliases
          (insert "\nAliases: " (mapconcat #'identity aliases ", ")))
        (when visibility
          (insert "\nVisibility: " visibility)))
      (visual-line-mode 1))))

(defun robe-doc-fontify-regions ()
  (let (last-pos)
    (while (not (eobp))
      (when last-pos (robe-doc-apply-rules last-pos (point)))
      (while (looking-at "\\(\n\\)+\\( +.*\n\\)+\\(\n\\|\\'\\)")
        (save-match-data
          (robe-doc-fontify-ruby (match-beginning 0) (match-end 0)))
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

(defun robe-doc-fontify-ruby (from to)
  (let ((syntax-propertize-function #'ruby-syntax-propertize-function)
        (font-lock-defaults (list ruby-font-lock-keywords nil nil))
        (font-lock-syntax-table ruby-font-lock-syntax-table)
        (font-lock-dont-widen t))
    (save-restriction
      (narrow-to-region from to)
      (font-lock-fontify-region from to))))

(defun robe-doc-fontify-c-string (string)
  (with-temp-buffer
    (insert string)
    (let ((delay-mode-hooks t))
      (c-mode))
    (run-hooks 'font-lock-mode-hook)
    (font-lock-fontify-buffer)
    (buffer-string)))

(defun robe-toggle-source (button)
  (let* ((end (button-end button))
         (value (get-text-property end 'invisible))
         (inhibit-read-only t))
    (put-text-property end (point-max) 'invisible (not value))))

(defun robe-signature (spec &optional arg-num)
  (concat
   (mapconcat (lambda (s) (propertize s 'face font-lock-type-face))
              (split-string (robe-spec-module spec) "::" t) "::")
   (if (robe-spec-inst-p spec) "#" ".")
   (propertize (robe-spec-method spec) 'face font-lock-function-name-face)
   (robe-signature-params (robe-spec-params spec) arg-num)))

(defun robe-signature-params (params &optional arg-num)
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
    (concat "(" (mapconcat #'identity (nreverse args) ", ") ")")))

(defun robe-doc-for (spec)
  (apply 'robe-request "doc_for" (subseq spec 0 3)))

(defun robe-call-at-point ()
  (let ((state (syntax-ppss)) (start (point))
        in-arglist)
    (unless (nth 4 state)
      (when (nth 3 state) (goto-char (nth 8 state)))
      (unless (ignore-errors (save-excursion
                               (eq ?. (char-before
                                       (beginning-of-thing 'symbol)))))
        (when (or (robe-call-goto-paren state)
                  (robe-call-goto-parenless))
          (setq in-arglist t)))
      (let ((thing (thing-at-point 'symbol)))
        (when (and thing
                   (or (string= thing "super")
                       (not (memq (get-text-property 0 'face thing)
                                  (list font-lock-function-name-face
                                        font-lock-keyword-face)))))
          (cons thing (when in-arglist
                        (robe-call-arg-num (point) start))))))))

(defun robe-call-goto-paren (state)
  (when (and (plusp (nth 0 state))
             (eq (char-after (nth 1 state)) ?\())
    (goto-char (nth 1 state))
    (skip-chars-backward " ")))

(defun robe-call-goto-parenless ()
  (let ((table (copy-syntax-table (syntax-table)))
        (punct (string-to-syntax "."))
        (start (point))
        point)
    (modify-syntax-entry ?! "_" table)
    (modify-syntax-entry ?@ "_" table)
    (with-syntax-table table
      (save-excursion
        (catch 'stop
          (unless (eobp) (forward-char 1))
          (while (re-search-backward "\\S-\\([ ]+\\)\\S-" nil t)
            (let ((state (parse-partial-sexp start (match-beginning 1))))
              (goto-char (match-beginning 1))
              (when (and (zerop (nth 0 state)) (not (nth 8 state)))
                (cond
                 ((save-match-data (string-match-p ";\\|=[^>]" (match-string 0)))
                  (throw 'stop t))
                 ((eq (char-after (match-beginning 0)) ?\n)
                  (unless (eq (char-before (match-beginning 0)) ?,)
                    (throw 'stop t)))
                 ((eq (char-after (match-beginning 0)) ?:) nil)
                 ((not (or (eq (syntax-after (match-beginning 0)) punct)
                           (eq (syntax-after (match-end 1)) punct)))
                  (setq point (match-beginning 1))
                  (throw 'stop t))))))))
      (when point (goto-char point)))))

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
  (when robe-running
    (save-excursion
      (let* ((call (robe-call-at-point))
             (thing (car call))
             (arg-num (cdr call))
             (url-show-status nil)
             (robe-max-retries 0))
        (when (and thing (not (robe-const-p thing)))
          (let* ((robe-jump-conservative t)
                 (list (loop for spec in (robe-jump-modules thing)
                             when (robe-spec-module spec) collect spec)))
            (when (consp list)
              (let* ((spec (car list))
                     (doc (robe-doc-for spec))
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
                     (sig (robe-signature spec arg-num))
                     (msg (format "%s %s" sig summary)))
                (substring msg 0 (min (frame-width) (length msg)))))))))))

(defun robe-complete-at-point ()
  (let ((bounds (bounds-of-thing-at-point 'symbol))
        (fn (completion-table-dynamic #'robe-complete-thing)))
    (if bounds
        (list (car bounds) (cdr bounds) fn
              :annotation-function #'robe-complete-annotation
              :exit-function #'robe-complete-exit)
      (list (point) (point) fn))))

(defvar robe-specs-cache nil)

(defmacro robe-with-cached-spec (method &rest body)
  (declare (indent 1) (debug t))
  `(when robe-specs-cache
     (let ((spec (gethash ,method robe-specs-cache)))
       (when spec
         ,@body))))

(defun robe-complete-annotation (thing)
  (robe-with-cached-spec thing
    (let ((params (robe-signature-params (robe-spec-params spec))))
      (if robe-highlight-capf-candidates
          params
        (substring-no-properties params)))))

(defun robe-complete-exit (&rest _)
  (setq robe-specs-cache nil))

(defun robe-complete-thing (thing)
  (setq this-command 'robe-complete-thing)
  (robe-start)
  (if (robe-const-p thing)
      (progn
        (robe-complete-exit)
        (robe-request "complete_const" thing (car (robe-context))))
    (destructuring-bind (target module instance _) (robe-call-context)
      (setq robe-specs-cache (make-hash-table :test 'equal))
      (mapcar (lambda (spec)
                (let ((method (robe-spec-method spec))
                      case-fold-search)
                  (puthash method spec robe-specs-cache)
                  (if robe-highlight-capf-candidates
                      (propertize method 'face
                                  (if (string-match "\\`[A-Z]" method)
                                      'font-lock-type-face
                                    'font-lock-function-name-face))
                    method)))
              (reverse
               (robe-request "complete_method" thing target module instance))))))

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
  (add-hook 'completion-at-point-functions 'robe-complete-at-point nil t)
  (set (make-local-variable 'eldoc-documentation-function) 'robe-eldoc)
  (eldoc-add-command 'robe-complete-thing)
  (turn-on-eldoc-mode))

(provide 'robe)
;;; robe.el ends here
