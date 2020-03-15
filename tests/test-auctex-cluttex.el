;;; -*- lexical-binding: t -*-

(require 'auctex-cluttex)
(require 'buttercup)
(require 'with-simulated-input)

(defvar auctex-cluttex-test-dir (if load-file-name
                                    (file-name-directory load-file-name)
                                  default-directory))

(describe "Function `auctex-cluttex-mode'"
  :var ((file (expand-file-name "sample/main.tex" auctex-cluttex-test-dir))
        (pdf  (expand-file-name "sample/main.pdf" auctex-cluttex-test-dir))
        (log  (expand-file-name "sample/main.log" auctex-cluttex-test-dir)))
  (before-all
    (when (file-exists-p pdf)
      (delete-file pdf)))
  (before-each
    (spy-on 'auctex-cluttex--TeX-run-ClutTeX :and-call-through))
  (after-each
    (auctex-cluttex-mode 0))

  (it "should enable `auctex-cluttex-mode'"
    (expect auctex-cluttex-mode :to-be nil)
    (auctex-cluttex-mode 1)
    (expect auctex-cluttex-mode :to-be t)
    (expect TeX-command-list :to-contain auctex-cluttex-ClutTeX-command))

  (it "should disable `auctex-cluttex-mode'"
    (auctex-cluttex-mode 1)
    (auctex-cluttex-mode 0)
    (expect auctex-cluttex-mode :to-be nil)
    (expect TeX-command-list :not :to-contain auctex-cluttex-ClutTeX-command))

  (it "should fail to enable `auctex-cluttex-mode'"
    (let ((exec-path nil))
      (expect (auctex-cluttex-mode 1) :to-throw 'file-error)
      (expect auctex-cluttex-mode :to-be nil)
      (expect TeX-command-list :not :to-contain auctex-cluttex-ClutTeX-command)))

  (it "should run cluttex command"
    (auctex-cluttex-mode 1)
    (let* ((TeX-parse-self t)
           (buf (find-file-noselect file))
           (TeX-process-asynchronous nil))
      (with-current-buffer buf
        (with-simulated-input "ClutTeX RET"
          (call-interactively #'TeX-command-master))
        (expect 'auctex-cluttex--TeX-run-ClutTeX :to-have-been-called)
        (expect (file-exists-p pdf) :to-be t)
        (expect (file-exists-p log) :to-be nil))
      (kill-buffer buf))))


(describe "SyncTeX support"
  :var ((file    (expand-file-name "sample/main.tex" auctex-cluttex-test-dir))
        (pdf     (expand-file-name "sample/main.pdf" auctex-cluttex-test-dir))
        (log     (expand-file-name "sample/main.log" auctex-cluttex-test-dir))
        (synctex (expand-file-name "sample/main.synctex.gz" auctex-cluttex-test-dir)))
  (before-all
    (TeX-source-correlate-mode 1)
    (auctex-cluttex-mode 1)
    (when (file-exists-p pdf)
      (delete-file pdf)))
  (after-all
    (TeX-source-correlate-mode 0)
    (auctex-cluttex-mode 0)
    (when (file-exists-p synctex)
      (delete-file synctex)))

  (it "should run cluttex command with `--synctex=1' option"
    (let* ((TeX-parse-self t)
           (buf (find-file-noselect file))
           (TeX-process-asynchronous nil))
      (with-current-buffer buf
        (with-simulated-input "ClutTeX RET"
          (call-interactively #'TeX-command-master))
        (expect (file-exists-p pdf) :to-be t)
        (expect (file-exists-p synctex) :to-be t)
        (expect (file-exists-p log) :to-be nil))
      (kill-buffer buf))))


(describe "Function `auctex-cluttex-set-command-default'"
  :var ((file (expand-file-name "sample/main.tex" auctex-cluttex-test-dir))
        (pdf  (expand-file-name "sample/main.pdf" auctex-cluttex-test-dir))
        (log  (expand-file-name "sample/main.log" auctex-cluttex-test-dir)))
  (before-each
    (spy-on 'TeX-run-TeX)
    (spy-on 'auctex-cluttex--TeX-run-ClutTeX :and-call-through)
    (when (file-exists-p pdf)
      (delete-file pdf)))
  (after-each
    (auctex-cluttex-mode 0))

  (it "should not run cluttex command by default"
    (auctex-cluttex-mode 1)
    (let* ((TeX-parse-self t)
           (buf (find-file-noselect file))
           (TeX-process-asynchronous nil))
      (with-current-buffer buf
        (with-simulated-input "RET"
          (call-interactively #'TeX-command-master))
        (expect 'TeX-run-TeX :to-have-been-called)
        (expect 'auctex-cluttex--TeX-run-ClutTeX :not :to-have-been-called))
      (kill-buffer buf)))

  (it "should not run cluttex command by default if `auctex-cluttex-mode' disabled"
    (add-hook 'LaTeX-mode-hook
              #'auctex-cluttex-set-command-default)
    (let* ((TeX-parse-self t)
           (buf (find-file-noselect file))
           (TeX-process-asynchronous nil))
      (with-current-buffer buf
        (with-simulated-input "RET"
          (call-interactively #'TeX-command-master))
        (expect 'TeX-run-TeX :to-have-been-called)
        (expect 'auctex-cluttex--TeX-run-ClutTeX :not :to-have-been-called))
      (kill-buffer buf)))

  (it "should run cluttex command by default"
    (auctex-cluttex-mode 1)
    (add-hook 'LaTeX-mode-hook
              #'auctex-cluttex-set-command-default)
    (let* ((TeX-parse-self t)
           (buf (find-file-noselect file))
           (TeX-process-asynchronous nil))
      (with-current-buffer buf
        (with-simulated-input "RET"
          (call-interactively #'TeX-command-master))
        (expect 'TeX-run-TeX :not :to-have-been-called)
        (expect 'auctex-cluttex--TeX-run-ClutTeX :to-have-been-called)
        (expect (file-exists-p pdf) :to-be t)
        (expect (file-exists-p log) :to-be nil))
      (kill-buffer buf))))
