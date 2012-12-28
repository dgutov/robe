(require 'robe)

(defun robe-ac-doc (symbol)
  "Return popup documentation for auto-complete."
  (when (and robe-running robe-specs-cache)
    (let ((spec (gethash symbol robe-specs-cache)))
      (when spec
        (concat (robe-signature spec)
                "\n\n"
                (cdr (assoc 'docstring (robe-doc-for spec))))))))

;;;###autoload
(defun robe-ac-available ()
  "Return t if robe completions are available, otherwise nil."
  robe-mode)

(defun robe-ac-candidates ()
  "Return completion candidates for ac-prefix."
  (when robe-running
    (robe-complete-thing ac-prefix)))

;;;###autoload
(defconst ac-source-robe
  '((available . robe-ac-available)
    (candidates . robe-ac-candidates)
    (document . robe-ac-doc)
    (symbol . "r"))
  "Auto-complete completion source for Ruby using robe.")
