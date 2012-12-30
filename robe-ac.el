(eval-when-compile (require 'robe))

(defun robe-ac-doc (symbol)
  "Return popup documentation for `auto-complete'."
  (when robe-running
    (robe-with-cached-spec symbol
      (concat (robe-signature spec)
              "\n\n"
              (cdr (assoc 'docstring (robe-doc-for spec)))))))

;;;###autoload
(defun robe-ac-available ()
  "Return t if `robe-mode' completions are available, otherwise nil."
  (and (boundp 'robe-mode) robe-mode))

(defun robe-ac-candidates ()
  "Return completion candidates for `ac-prefix'."
  (require 'robe)
  (when robe-running
    (robe-complete-thing ac-prefix)))

;;;###autoload
(defconst ac-source-robe
  '((available . robe-ac-available)
    (candidates . robe-ac-candidates)
    (document . robe-ac-doc)
    (symbol . "r"))
  "`auto-complete' completion source for Ruby using `robe-mode'.")

(provide 'robe-ac)
