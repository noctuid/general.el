;;; general-non-existent-state.el --- For testing delayed keybindings.
;;; Commentary:
;;; Code:
(require 'evil)

(evil-define-state non-existent
  "I don't normally exist")

(provide 'general-non-existent-state)
;;; general-non-existent-state.el ends here
