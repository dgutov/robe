(eval-when-compile (require 'robe))

;;;###autoload
(defun company-robe (command &optional arg &rest ignore)
  "A `company-mode' completion back-end for `robe-mode'."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-robe))
    (prefix (and (boundp 'robe-mode)
                 robe-mode (robe-running-p)
                 (company-robe--prefix)))
    (candidates (robe-complete-thing arg))
    (meta (company-robe--meta arg))
    (post-completion
     (company-robe--post-completion arg))
    (location (let ((spec (company-robe--choose-spec arg)))
                (cons (robe-spec-file spec)
                      (robe-spec-line spec))))
    (kind (company-robe--kind arg))
    (annotation (robe-complete-annotation arg))
    (no-cache t)
    (match      (company-robe--match arg))
    (sorted t)
    (doc-buffer (let ((spec (company-robe--choose-spec arg)))
                  (when spec
                    (save-window-excursion
                      (robe-show-doc spec)
                      (message nil)
                      (get-buffer "*robe-doc*")))))))

(defun company-robe--post-completion (arg)
  (let ((ann (robe-complete-annotation arg))
        (pos (point))
        (num 1))
    (when ann
      (yas-expand-snippet
       (with-temp-buffer
         (insert ann)
         (goto-char (point-min))
         (while (re-search-forward
                 "\\([\(,] *\\)\\(\\(?:[\]*&.:\[]\\|\\sw\\|\\s_\\)+\\)"
                 nil 'move)
           (let ((comma (match-string 1))
                 (ident (match-string 2)))
             (replace-match
              (if (memq (aref ident 0) '(?\[ ?* ?&))
                  ""
                (prog1
                    (format "%s${%d:%s}" comma num ident)
                  (setq num (1+ num)))))))
         (when (= num 1)
           (backward-delete-char 1))
         (insert "$0")
         (buffer-string))
       pos pos nil))))

(defun company-robe--meta (completion)
  (if-let ((type (get-text-property 0 'robe-type completion)))
      (if-let ((vtype (get-text-property 0 'robe-variable-type completion)))
          (format "%s => %s" type (propertize vtype 'face 'font-lock-type-face))
        type)
    (let ((spec (car (robe-cached-specs completion))))
      (when spec (robe-signature spec)))))

(defun company-robe--prefix ()
  (let ((bounds (robe-complete-bounds)))
    (when (and bounds
               (equal (point) (cdr bounds))
               (robe-complete-symbol-p (car bounds)))
      (buffer-substring (car bounds) (cdr bounds)))))

(defun company-robe--choose-spec (thing)
  (let ((specs (robe-cached-specs thing)))
    (when specs
      (if (cdr specs)
          (let ((alist (cl-loop for spec in specs
                             for module = (robe-spec-module spec)
                             when module
                             collect (cons module spec))))
            (cdr (assoc (robe-completing-read "Module: " alist nil t) alist)))
        (car specs)))))

(defun company-robe--match (str)
  (company-flex-highlights company-prefix str))

(defun company-flex-highlights (input string)
  (let ((re
         (mapconcat
          (lambda (c)
            (format "[^%c]*\\(%s\\)" c (regexp-quote (string c))))
          input ""))
        (case-fold-search (company-call-backend 'ignore-case))
        res ref)
    (when (string-match re string)
      (setq ref (nthcdr 2 (match-data)))
      (while ref
        (push (cons (car ref) (cadr ref)) res)
        (setq ref (cddr ref)))
      res)))

(defun company-robe--kind (arg)
  (let (case-fold-search)
    (cond
     ((string-match "\\(\\`\\|::\\)[A-Z]\\([A-Z_0-9]*\\)?\\'" arg)
      (if (match-beginning 1)
          'constant
        'module))
     ((string-match-p "\\`@" arg)
      'variable)
     ((eq (get-text-property 0 'robe-type arg) 'variable)
      'value)
     (t 'method))))

(provide 'company-robe)
