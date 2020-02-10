;;; auctex-cluttex.el --- ClutTeX support for AUCTeX

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
;;    (require 'auctex-cluttex))
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

(unless (executable-find "cluttex")
  (error "Cannot find cluttex command"))


(setq TeX-command-list
      (append
       TeX-command-list
       '(("ClutTeX" "cluttex -e %(cluttexengine) %(cluttexbib) %(cluttexindex) %S %t"
          TeX-run-ClutTeX nil
          (plain-tex-mode latex-mode) :help "Run ClutTeX"))))

(setq TeX-expand-list-builtin
      (append
       TeX-expand-list-builtin
       '(("%(cluttexengine)"
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
         ("%(cluttexbib)"
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
         ("%(cluttexindex)"
          (lambda ()
            (cond
             ((LaTeX-index-entry-list)
              ;; TODO: makeglossaries support
              (format "--makeindex=%s"
                      (cond
                       ((memq TeX-engine '(uptex xetex luatex)) "upmendex")
                       ((eq TeX-engine 'ptex) "mendex")
                       (t "makeindex"))))
             (t "")))))))

(defun TeX-run-ClutTeX (name command file)
  "Create a process for NAME using COMMAND to convert FILE with ClutTeX."
  (let ((process (TeX-run-command name command file)))
    (setq TeX-sentinel-function #'TeX-ClutTeX-sentinel)
    (if TeX-process-asynchronous
        (progn
          (set-process-filter process #'TeX-ClutTeX-filter)
          process)
      (TeX-synchronous-sentinel name file process))))

(defun TeX-ClutTeX-filter (process string)
  "Filter to process PROCESS normal output STRING."
  (with-current-buffer (process-buffer process)
    (save-excursion
      (goto-char (process-mark process))
      (insert-before-markers (ansi-color-apply string))
      (set-marker (process-mark process) (point)))))

(defun TeX-ClutTeX-sentinel (_process _name)
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

(defun auctex-cluttex--TeX-command-default-advice (ret)
  "Advice to function `TeX-command-default'.
If RET is `TeX-command-BibTeX' or `TeX-command-Biber', return
`TeX-command-Show' only when variable `TeX-command-default' is ClutTeX."
  (if (and (equal TeX-command-default "ClutTeX")
           (member ret `(,TeX-command-BibTeX ,TeX-command-Biber)))
      TeX-command-Show
    ret))

(advice-add 'TeX-command-default :filter-return #'auctex-cluttex--TeX-command-default-advice)

(defun auctex-cluttex-set-command-default ()
  "Set variable `TeX-command-default' to ClutTeX."
  (setq TeX-command-default "ClutTeX"))

(provide 'auctex-cluttex)

;;; auctex-cluttex.el ends here
