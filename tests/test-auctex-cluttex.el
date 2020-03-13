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
        (log  (expand-file-name "sample/main.log" auctex-cluttex-test-dir))
        (TeX-parse-self t))
  (before-each
    (when (file-exists-p pdf)
      (delete-file pdf)))
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
    (let ((buf (find-file-noselect file)))
      (with-current-buffer buf
        (setq TeX-process-asynchronous nil)
        (with-simulated-input "ClutTeX RET"
          (call-interactively #'TeX-command-master))
        (expect (file-exists-p pdf) :to-be t)
        (expect (file-exists-p log) :to-be nil)
        (kill-buffer buf)))))

(describe "Function `auctex-cluttex-set-command-default'"
  :var ((file (expand-file-name "sample/main.tex" auctex-cluttex-test-dir))
        (pdf  (expand-file-name "sample/main.pdf" auctex-cluttex-test-dir))
        (log  (expand-file-name "sample/main.log" auctex-cluttex-test-dir))
        (TeX-parse-self t))
  (before-all
    (auctex-cluttex-mode 1))
  (after-all
    (auctex-cluttex-mode 0))
  (before-each
    (when (file-exists-p pdf)
      (delete-file pdf)))
  (it "should run cluttex command by default"
    (let ((buf (find-file-noselect file)))
      (with-current-buffer buf
        (auctex-cluttex-set-command-default)
        (setq TeX-process-asynchronous nil)
        (with-simulated-input "RET"
          (call-interactively #'TeX-command-master))
        (expect (file-exists-p pdf) :to-be t)
        (expect (file-exists-p log) :to-be nil)
        (kill-buffer buf)))))
