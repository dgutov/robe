;;; zossima.el --- Ruby from Emacs

;; Copyright © 2012 Phil Hagelberg

;; Author: Phil Hagelberg
;; URL: https://github.com/technomancy/zossima
;; Version: 0.1
;; Created: 2012-10-24
;; Keywords: ruby convenience
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

;; (add-hook 'inf-ruby-mode-hook 'zossima-mode)
;;
;;  - M-. to jump to a definition
;;  - M-, to jump back

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
(require 'ido)
(require 'cl)

(defvar zossima-ruby-path
  (let ((current (or load-file-name (buffer-file-name))))
    (concat (file-name-directory current) "/zossima.rb"))
  "Path to file containing Ruby implementation of Zossima")

(defvar zossima-port 24959)

(defvar zossima-regex "^\\([A-Z][A-Za-z:]+\\)\\([#\\.]\\)\\([a-z_]+\\)$")

(defun zossima-start ()
  "Ensure remote process has Zossima started."
  (comint-send-string (inf-ruby-proc)
                      (format "load '%s' unless defined? Zossima\n"
                              zossima-ruby-path))
  (comint-send-string (inf-ruby-proc)
                      (format "Zossima.start(%s)\n" zossima-port)))

(defun zossima-request (endpoint &rest args)
  (let* ((url (format "http://localhost:%s/%s/%s" zossima-port endpoint
                      (mapconcat 'identity args "/")))
         (response-buffer (url-retrieve-synchronously url))
         (value (with-current-buffer response-buffer
                  (goto-char (point-min))
                  (search-forward "\n\n")
                  (let ((json-array-type 'list))
                    (json-read)))))
    (kill-buffer response-buffer)
    value))

(defun zossima-jump ()
  "Jump to method definition."
  (interactive)
  (zossima-start)
  (let* ((classes (zossima-request "classes"))
         (class (ido-completing-read "Class: " classes))
         (targets (zossima-request "targets" class))
         (target (ido-completing-read "Method: " targets))
         (_ (string-match zossima-regex target))
         (class (match-string 1 target))
         (type (if (string= "#" (match-string 2 target)) "instance" "class"))
         (method (match-string 3 target))
         (location (zossima-request "location" class type method)))
    (ring-insert find-tag-marker-ring (point-marker))
    (find-file (nth 0 location))
    (goto-char (point-min))
    (forward-line (1- (nth 1 location)))
    (back-to-indentation)))

(defvar zossima-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map ruby-mode-map)
    (define-key map (kbd "M-.") 'zossima-jump)
    (define-key map (kbd "M-,") 'pop-tag-mark)
    map))

;;;###autoload
(define-minor-mode zossima-mode
  "Improved navigation for Ruby"
  nil " zossima" zossima-mode-map)

(provide 'zossima)
;;; zossima.el ends here
