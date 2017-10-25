;;; general-keymap-autoload.el --- For testing "autoloaded" keymaps. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(defvar general-autoload-map (make-sparse-keymap))
(define-key general-autoload-map "f" #'forward-char)

(provide 'general-keymap-autoload)
;;; general-keymap-autoload.el ends here
