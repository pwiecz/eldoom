;;; eldoom-render-golden-test.el --- ERT tests for render goldens -*- lexical-binding:t -*-

;; Copyright (C) 2026 Piotr Wieczorek <pwiecz@gmail.com>

;;; Code:

(require 'ert)
(require 'eldoom-render-golden)

(ert-deftest erg-render-goldens-match ()
  (skip-unless (erg-wad-available-p))
  (dolist (result (erg-check-all-goldens))
    (should (plist-get result :identical-p))))

(provide 'eldoom-render-golden-test)
;;; eldoom-render-golden-test.el ends here
