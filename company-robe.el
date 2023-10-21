(eval-when-compile (require 'robe))

;;;###autoload
(defun company-robe (command &optional arg &rest rest)
  "A `company-mode' completion back-end for `robe-mode'."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-robe))
    (prefix (and (boundp 'robe-mode)
                 robe-mode (robe-running-p)
                 (company-robe--prefix)))
    (candidates (robe-complete-thing arg))
    (duplicates t)
    (meta (company-robe--meta arg rest))  ; To support nils too
    (location (let ((spec (company-robe--choose-spec arg)))
                (cons (robe-spec-file spec)
                      (robe-spec-line spec))))
    (kind (company-robe--kind arg))
    (annotation (robe-complete-annotation arg))
    (variants (mapcar #'robe-spec-module (robe-cached-specs arg)))
    (doc-buffer (let ((spec (company-robe--choose-spec arg rest))
                      (inhibit-redisplay t)
                      ;; XXX: Maybe revisit company-mode/company-mode#548.
                      (timer-list nil))
                  (when spec
                    (save-window-excursion
                      (robe-show-doc spec)
                      (message nil)
                      (get-buffer "*robe-doc*")))))))

(defun company-robe--meta (completion &optional rest)
  (if-let ((type (get-text-property 0 'robe-type completion)))
      (if-let ((vtype (get-text-property 0 'robe-variable-type completion)))
          (format "%s => %s" type (propertize vtype 'face 'font-lock-type-face))
        type)
    (let* ((specs (robe-cached-specs completion))
           (spec (if rest
                     (cl-find (car rest) specs :key #'robe-spec-module)
                   (car specs)))
           (count (length specs))
           (variant-format (if (cdr specs) (format "%i/%i "
                                                   (1+ (- count (length (memq spec specs))))
                                                   count))))
      (when spec (concat variant-format (robe-signature spec))))))

(defun company-robe--prefix ()
  (let ((bounds (robe-complete-bounds)))
    (when (and bounds
               (equal (point) (cdr bounds))
               (robe-complete-symbol-p (car bounds)))
      (buffer-substring (car bounds) (cdr bounds)))))

(defun company-robe--choose-spec (thing &optional rest)
  (let ((specs (robe-cached-specs thing)))
    (when specs
      (if (cdr specs)
          (if rest
              (cl-find (car rest) specs :key #'robe-spec-module)
            (let ((alist (cl-loop for spec in specs
                                  for module = (robe-spec-module spec)
                                  when module
                                  collect (cons module spec))))
              (cdr (assoc (robe-completing-read "Module: " alist nil t)
                          alist))))
        (car specs)))))

(defun company-robe--kind (arg)
  (let (case-fold-search)
    (cond
     ((string-match "\\(?:\\`\\|::\\)\\(?:[A-Z_0-9]*\\|\\([A-Z][A-Z_a-z0-9]*\\)\\)\\'" arg)
      (if (match-beginning 1)
          'module
        'constant))
     ((string-match-p "\\`@" arg)
      'variable)
     ((get-text-property 0 'robe-type arg)
      'value)
     (t 'method))))

(provide 'company-robe)
