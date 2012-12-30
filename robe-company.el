(eval-when-compile (require 'robe))

;;;###autoload
(defun company-robe (command &optional arg)
  "A `company-mode' completion back-end for `robe-mode'."
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-robe))
    (prefix (and (boundp 'robe-mode)
                 robe-mode robe-running
                 (or (thing-at-point 'symbol)
                     "")))
    (candidates (robe-complete-thing arg))
    (duplicates t)
    (meta (robe-with-cached-spec arg
            (robe-signature spec)))
    (location (robe-with-cached-spec arg
                (cons (robe-spec-file spec)
                      (robe-spec-line spec))))
    (doc-buffer (robe-with-cached-spec arg
                  (save-window-excursion
                    (robe-show-doc spec)
                    (get-buffer "*robe-doc*"))))))

(provide 'robe-company)
