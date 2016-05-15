;;; general-tests.el --- Tests for general.el.
;;; Commentary:
;;; Code:
(require 'general)
(require 'evil)

(defun general-test-keys (keymap &rest maps)
  (declare (indent 1))
  (let (key func)
    (while (setq key (pop maps)
                 func (pop maps))
      (should (eq (lookup-key keymap key)
                  func)))))

(defun general-test-evil-keys (state keymap &rest maps)
  (declare (indent 2))
  (let (key func)
    (while (setq key (pop maps)
                 func (pop maps))
      (should (eq (lookup-key (evil-get-auxiliary-keymap keymap state t) key)
                  func)))))

(ert-deftest general-define-key ()
  ;; test global keymap
  (general-define-key "a" #'a)
  (general-test-keys (current-global-map) "a" #'a)
  ;; basic tests
  (let ((general-test-map (make-sparse-keymap))
        (evil-normal-state-map (make-sparse-keymap))
        (evil-insert-state-map (make-sparse-keymap)))
    (general-define-key :keymaps 'general-test-map
                        "a" #'a
                        ;; test implicit kbd
                        "SPC a" #'spc-a)
    (general-test-keys general-test-map
      "a" #'a
      (kbd "SPC a") #'spc-a)
    (general-define-key :keymaps 'general-test-map
                        :states 'normal
                        "a" #'a)
    (general-test-evil-keys 'normal general-test-map
      "a" #'a)
    ;; test unbinding
    (general-define-key :keymaps 'general-test-map
                        "a" nil)
    (general-test-keys general-test-map "a" nil)
    (let ((general-implicit-kbd nil))
      (general-define-key :keymaps 'general-test-map
                          "b c" #'b-space-c)
      (general-test-keys general-test-map
        (kbd "b SPC c") #'b-space-c))
    ;; test states and prefixes (and multiple keymaps)
    (general-define-key :prefix ","
                        :keymaps 'general-test-map
                        "a" #'comma-a)
    (general-test-keys general-test-map ",a" #'comma-a)
    (general-define-key :prefix ","
                        :keymaps '(evil-normal-state-map
                                   evil-insert-state-map)
                        "a" #'comma-a)
    (general-test-keys evil-normal-state-map ",a" #'comma-a)
    (general-test-keys evil-insert-state-map ",a" #'comma-a)
    (general-define-key :prefix ","
                        :keymaps 'general-test-map
                        :states '(normal insert)
                        "a" #'comma-a)
    (general-test-evil-keys 'normal general-test-map ",a" #'comma-a)
    (general-test-evil-keys 'insert general-test-map ",a" #'comma-a)
    (general-define-key :prefix ","
                        :global-prefix "C-c"
                        :non-normal-prefix "C-a"
                        :keymaps '(evil-normal-state-map
                                   evil-insert-state-map)
                        "a" #'prefix-a)
    (general-test-keys evil-normal-state-map ",a" #'prefix-a)
    (general-test-keys evil-normal-state-map (kbd "C-c a") #'prefix-a)
    (general-test-keys evil-insert-state-map (kbd "C-a a") #'prefix-a)
    (general-test-keys evil-insert-state-map (kbd "C-c a") #'prefix-a)
    (general-define-key :prefix ","
                        :global-prefix "C-c"
                        :non-normal-prefix "C-a"
                        :keymaps 'general-test-map
                        :states '(normal insert)
                        "a" #'prefix-a)
    (general-test-evil-keys 'normal general-test-map ",a" #'prefix-a)
    (general-test-evil-keys 'normal general-test-map (kbd "C-c a") #'prefix-a)
    (general-test-evil-keys 'insert general-test-map (kbd "C-a a") #'prefix-a)
    (general-test-evil-keys 'insert general-test-map (kbd "C-c a") #'prefix-a))
  ;; test delaying keybindings
  (general-define-key :keymaps 'general-delay-map
                      "C-t" #'c-t)
  (should (not (boundp 'general-delay-map)))
  (require 'general-delay-test)
  (general-test-keys general-delay-map (kbd "C-t") #'c-t))

(ert-deftest general-emacs-define-key ()
  (let ((general-test-map (make-sparse-keymap))
        (general-test2-map (make-sparse-keymap)))
    (general-emacs-define-key general-test-map
      :prefix "C-c"
      "a" #'c-c-a)
    (general-test-keys general-test-map (kbd "C-c a") #'c-c-a)
    (general-emacs-define-key 'general-test-map
      "C-c b" #'c-c-b)
    (general-test-keys general-test-map (kbd "C-c b") #'c-c-b)
    (general-emacs-define-key '(general-test-map
                                general-test2-map)
      "C-t" #'c-t)
    (general-test-keys general-test-map (kbd "C-t") #'c-t)
    (general-test-keys general-test2-map (kbd "C-t") #'c-t)))

(ert-deftest general-evil-define-key ()
  (let ((general-test-map (make-sparse-keymap))
        (general-test2-map (make-sparse-keymap)))
    (general-evil-define-key 'normal general-test-map
      :prefix "C-c"
      "a" #'c-c-a)
    (general-test-evil-keys 'normal general-test-map (kbd "C-c a") #'c-c-a)
    (general-evil-define-key 'normal 'general-test-map
      "C-c b" #'c-c-b)
    (general-test-evil-keys 'normal general-test-map (kbd "C-c b") #'c-c-b)
    (general-evil-define-key
        '(normal
          insert)
        '(general-test-map
          general-test2-map)
      "C-t" #'c-t)
    (general-test-evil-keys 'normal general-test-map (kbd "C-t") #'c-t)
    (general-test-evil-keys 'normal general-test2-map (kbd "C-t") #'c-t)
    (general-test-evil-keys 'insert general-test-map (kbd "C-t") #'c-t)
    (general-test-evil-keys 'insert general-test2-map (kbd "C-t") #'c-t)))

(ert-deftest general-simulate-keys ()
  (let ((general-test-map (make-sparse-keymap)))
    (evil-define-key 'normal general-test-map
      "n" (general-simulate-keys "C-n")
      "N" (general-simulate-keys "C-n" t)
      (kbd "SPC") (general-simulate-keys "C-c" nil "" general-C-c))
    (general-test-evil-keys 'normal general-test-map
                            "n" #'general-simulate-C-n
                            "N" #'general-simulate-C-n-in-emacs-state
                            (kbd "SPC") #'general-C-c)
    ;; TODO: add a lispy-with like test for the effect
    ))

(provide 'general-tests)
;;; general-tests.el ends here
