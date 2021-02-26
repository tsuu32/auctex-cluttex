;;; -*- lexical-binding: t -*-

(require 'auctex-cluttex)
(require 'buttercup)
(require 'with-simulated-input)

(defvar auctex-cluttex-test-dir (if load-file-name
                                    (file-name-directory load-file-name)
                                  default-directory))

(describe "Function `auctex-cluttex-mode'"
  :var ((tex (expand-file-name "sample/main.tex" auctex-cluttex-test-dir))
        (pdf (expand-file-name "sample/main.pdf" auctex-cluttex-test-dir))
        (log (expand-file-name "sample/main.log" auctex-cluttex-test-dir)))
  (before-all
    (when (file-exists-p pdf)
      (delete-file pdf)))
  (before-each
    (spy-on 'auctex-cluttex--TeX-run-ClutTeX :and-call-through))

  (it "should enable `auctex-cluttex-mode'"
    (with-temp-buffer
      (expect auctex-cluttex-mode :to-be nil)
      (auctex-cluttex-mode 1)
      (expect auctex-cluttex-mode :to-be t)
      (expect TeX-command-list :to-contain auctex-cluttex-ClutTeX-command)))

  (it "should disable `auctex-cluttex-mode'"
    (with-temp-buffer
      (auctex-cluttex-mode 1)
      (auctex-cluttex-mode 0)
      (expect auctex-cluttex-mode :to-be nil)
      (expect TeX-command-list :not :to-contain auctex-cluttex-ClutTeX-command)))

  (it "should run cluttex command synchronously"
    (let* ((TeX-parse-self t)
           (buf (find-file-noselect tex))
           (TeX-process-asynchronous nil))
      (with-current-buffer buf
        (auctex-cluttex-mode 1)
        (with-simulated-input "ClutTeX RET"
          (call-interactively #'TeX-command-master))
        (expect 'auctex-cluttex--TeX-run-ClutTeX :to-have-been-called)
        (expect (file-exists-p pdf) :to-be t)
        (expect (file-exists-p log) :to-be nil))
      (kill-buffer buf)))

  (it "should not run cluttex command by default"
    (let* ((buf (find-file-noselect tex)))
      (with-current-buffer buf
        (auctex-cluttex-mode 1)
        (auctex-cluttex-mode 0)
        (expect TeX-command-default :not :to-equal "ClutTeX"))
      (kill-buffer buf)))

  (it "should run cluttex command by default"
    (let* ((buf (find-file-noselect tex)))
      (with-current-buffer buf
        (auctex-cluttex-mode 1)
        (expect TeX-command-default :to-equal "ClutTeX"))
      (kill-buffer buf))))


(describe "SyncTeX support"
  :var ((tex     (expand-file-name "sample/main.tex" auctex-cluttex-test-dir))
        (pdf     (expand-file-name "sample/main.pdf" auctex-cluttex-test-dir))
        (log     (expand-file-name "sample/main.log" auctex-cluttex-test-dir))
        (synctex (expand-file-name "sample/main.synctex.gz" auctex-cluttex-test-dir)))
  (before-all
    (TeX-source-correlate-mode 1)
    (when (file-exists-p pdf)
      (delete-file pdf))
    (when (file-exists-p synctex)
      (delete-file synctex)))
  (after-all
    (TeX-source-correlate-mode 0))

  (it "should run cluttex command with `--synctex=1' option"
    (let* ((TeX-parse-self t)
           (buf (find-file-noselect tex))
           (TeX-process-asynchronous nil))
      (with-current-buffer buf
        (auctex-cluttex-mode 1)
        (with-simulated-input "ClutTeX RET"
          (call-interactively #'TeX-command-master))
        (expect (file-exists-p pdf) :to-be t)
        (expect (file-exists-p synctex) :to-be t)
        (expect (file-exists-p log) :to-be nil))
      (kill-buffer buf))))
