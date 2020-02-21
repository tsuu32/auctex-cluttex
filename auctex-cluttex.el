;;; auctex-cluttex.el --- ClutTeX support for AUCTeX -*- lexical-binding: t -*-

;; Copyright (C) 2020 by Masahiro Nakamura

;; Author: Masahiro Nakamura <tsuucat@icloud.com>
;; Version: 0.1.0
;; URL: https://github.com/tsuu32/auctex-cluttex
;; Package-Requires: ((emacs "24.4") (auctex "12.2"))
;; Keywords: tex

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides ClutTeX support for AUCTeX package.

;; To use this package, add following code to your init file.
;;
;;  (with-eval-after-load 'tex
;;    (auctex-cluttex-mode))
;;
;; If you want to use ClutTeX as default command, add following code
;; to your init file.
;;
;;  (add-hook 'plain-TeX-mode-hook
;;            #'auctex-cluttex-set-command-default)
;;  (add-hook 'LaTeX-mode-hook
;;            #'auctex-cluttex-set-command-default)
;;

;;; Code:

(require 'tex)
(require 'latex)
(require 'tex-buf)


(defgroup auctex-cluttex nil
  "ClutTeX support for AUCTeX."
  :group 'TeX-command
  :prefix "auctex-cluttex-")

(defcustom auctex-cluttex-program "cluttex"
  "Name of cluttex command (usually `cluttex')."
  :group 'auctex-cluttex
  :type 'file)


(defvar auctex-cluttex-ClutTeX-command
  '("ClutTeX" "cluttex -e %(cluttexengine) %(cluttexbib) %(cluttexindex) %S %t"
    auctex-cluttex--TeX-run-ClutTeX nil
    (plain-tex-mode latex-mode) :help "Run ClutTeX")
  "ClutTeX command element.  See `TeX-command-list'.")

(defvar auctex-cluttex-cluttexengine-expand
  '("%(cluttexengine)"
    (lambda ()
      (format "%s%stex"
              (cond
               ((eq TeX-engine 'default) "pdf")
               ((eq TeX-engine 'xetex) "xe")
               ((eq TeX-engine 'luatex) "lua")
               ((eq TeX-engine 'ptex) "p")
               ((eq TeX-engine 'uptex) "up"))
              (cond
               ((eq major-mode 'plain-tex-mode) "")
               ((eq major-mode 'latex-mode) "la")))))
  "TeX engine detector for `auctex-cluttex-ClutTeX-command'.
See `TeX-expand-list-builtin'.")

(defvar auctex-cluttex-cluttexbib-expand
  '("%(cluttexbib)"
    (lambda ()
      (cond
       ((LaTeX-bibliography-list)
        (if LaTeX-using-Biber
            "--biber"
          (format "--bibtex=%s"
                  (cond
                   ((eq TeX-engine 'uptex) "upbibtex")
                   ((eq TeX-engine 'ptex) "pbibtex")
                   (t "bibtex")))))
       (t ""))))
  "BibTeX command detector for `auctex-cluttex-ClutTeX-command'.
See `TeX-expand-list-builtin'.")

(defvar auctex-cluttex-cluttexindex-expand
  '("%(cluttexindex)"
    (lambda ()
      (cond
       ((LaTeX-index-entry-list)
        ;; TODO: makeglossaries support
        (format "--makeindex=%s"
                (cond
                 ((memq TeX-engine '(uptex xetex luatex)) "upmendex")
                 ((eq TeX-engine 'ptex) "mendex")
                 (t "makeindex"))))
       (t ""))))
  "MakeIndex command detector for `auctex-cluttex-ClutTeX-command'.
See `TeX-expand-list-builtin'.")

(defun auctex-cluttex--TeX-run-ClutTeX (name command file)
  "Create a process for NAME using COMMAND to convert FILE with ClutTeX."
  (let ((process (TeX-run-command name command file)))
    (setq TeX-sentinel-function #'auctex-cluttex--TeX-ClutTeX-sentinel)
    (if TeX-process-asynchronous
        (progn
          (set-process-filter process #'auctex-cluttex--TeX-ClutTeX-filter)
          process)
      (TeX-synchronous-sentinel name file process))))

(defun auctex-cluttex--TeX-ClutTeX-filter (process string)
  "Filter to process PROCESS normal output STRING."
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (process-mark process))
      (insert-before-markers (ansi-color-apply string))
      (set-marker (process-mark process) (point)))))

(defun auctex-cluttex--TeX-ClutTeX-sentinel (_process _name)
  "Cleanup TeX output buffer after running ClutTeX."
  (unless TeX-process-asynchronous
    (ansi-color-apply-on-region (point-min) (point-max)))
  (goto-char (point-max))
  (cond
   ((search-backward "TeX Output exited abnormally" nil t)
    (message "ClutTeX failed.  Type `%s' to display output."
	     (substitute-command-keys
              "\\<TeX-mode-map>\\[TeX-recenter-output-buffer]")))
   (t
    (if (with-current-buffer TeX-command-buffer TeX-PDF-mode)
	(setq TeX-output-extension "pdf"
	      TeX-command-next TeX-command-Show))
    (message "ClutTeX finished successfully."))))


;;;###autoload
(define-minor-mode auctex-cluttex-mode
  "Toggle ClutTeX support for AUCTeX (AUCTeX ClutTeX mode).
With a prefix argument ARG, enable AUCTeX ClutTeX mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

When AUCTeX ClutTeX mode is enabled, `auctex-cluttex-ClutTeX-command'
is added to `TeX-command-list'."
  :global t
  (if auctex-cluttex-mode
      (unless (memq auctex-cluttex-ClutTeX-command TeX-command-list)
        (unless (executable-find auctex-cluttex-program)
          (setq auctex-cluttex-mode nil)
          (signal 'file-error (list "Searching for program"
                                    auctex-cluttex-program "no such file")))
        (setq TeX-command-list
              (append (butlast TeX-command-list 1)
                      (list auctex-cluttex-ClutTeX-command)
                      (last TeX-command-list)))
        (push auctex-cluttex-cluttexengine-expand TeX-expand-list-builtin)
        (push auctex-cluttex-cluttexbib-expand TeX-expand-list-builtin)
        (push auctex-cluttex-cluttexindex-expand TeX-expand-list-builtin))
    (setq TeX-command-list
          (cl-remove-if (lambda (item)
                          (eq item auctex-cluttex-ClutTeX-command))
                        TeX-command-list))
    (setq TeX-expand-list-builtin
          (cl-remove-if (lambda (item)
                          (or (eq item auctex-cluttex-cluttexengine-expand)
                              (eq item auctex-cluttex-cluttexbib-expand)
                              (eq item auctex-cluttex-cluttexindex-expand)))
                        TeX-expand-list-builtin))))

(defun auctex-cluttex--TeX-command-default-advice (retval)
  "Advice to function `TeX-command-default'.
If RETVAL is `TeX-command-BibTeX' or `TeX-command-Biber', return
`TeX-command-Show' only when variable `TeX-command-default' is
ClutTeX.  That's because ClutTeX does not output bbl file in
`TeX-master-directory'."
  (if (and (equal TeX-command-default "ClutTeX")
           (member retval `(,TeX-command-BibTeX ,TeX-command-Biber)))
      TeX-command-Show
    retval))

;;;###autoload
(defun auctex-cluttex-set-command-default ()
  "Set variable `TeX-command-default' to ClutTeX."
  (when auctex-cluttex-mode
    (setq TeX-command-default "ClutTeX")
    (advice-add 'TeX-command-default :filter-return
                #'auctex-cluttex--TeX-command-default-advice)))

(provide 'auctex-cluttex)

;;; auctex-cluttex.el ends here
