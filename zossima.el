;;; zossima.el --- Ruby from Emacs

;; Copyright (C) 2012 Phil Hagelberg

;; Author: Phil Hagelberg
;; URL: http://www.emacswiki.org/cgi-bin/wiki/Zossima
;; Version: 0.1
;; Created: 2012-10-24
;; Keywords: ruby convenience
;; EmacsWiki: Zossima
;; Package-Requires: ((inf-ruby "2.2.3"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:



;;; Install

;; M-x package-install zossima

;;; Usage

;; (add-hook 'inf-ruby-mode-hook 'zossima-enable)
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

(defvar zossima-ruby-path
  (let ((current (or load-file-name (buffer-file-name))))
    (concat (file-name-directory current) "/zossima.rb"))
  "Path to file containing Ruby implementation of Zossima")

(defvar zossima-port 24959)

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
                  (json-read))))
    (kill-buffer response-buffer)
    value))

(defun zossima-jump ()
  "Jump to method definition."
  (interactive)
  (zossima-start)
  (let* ((location (zossima-request "location"
                                    (read-from-minibuffer "Class: ")
                                    (read-from-minibuffer "Method: "))))
    (ring-insert find-tag-marker-ring (point-marker))
    (find-file (aref location 0))
    (goto-char (point-min))
    (forward-line (1- (aref location 1)))
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
