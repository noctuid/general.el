;;; general-tests.el --- Tests for general.el.
;;; Commentary:
;;; Code:
(require 'general)
(require 'evil)
(require 'use-package)
(require 'which-key)

(defun general-test-keys (keymap &rest maps)
  (declare (indent 1))
  (let (key func)
    (while (setq key (pop maps))
      (setq func (pop maps))
      (should (eq (lookup-key keymap key)
                  func)))))

(defun general-test-evil-keys (state keymap &rest maps)
  (declare (indent 2))
  (let (key func)
    (while (setq key (pop maps))
      (setq func (pop maps))
      (should (eq (lookup-key (evil-get-auxiliary-keymap keymap state t) key)
                  func)))))

(defmacro general-with (in &rest body)
  "This is `lispy-with' modified for general.
Note that | is considered to be \"on\" a character, meaning that it is included
in a visual selection. ~ on the other hand is not considered to be on a
character, so when it represents the region end, the character after it is not
considered as part of the region."
  (declare (indent 1))
  `(let ((temp-buffer (generate-new-buffer " *temp*")))
     (save-window-excursion
       (unwind-protect
            (progn
              (switch-to-buffer temp-buffer)
              (emacs-lisp-mode)
              (transient-mark-mode 1)
              (evil-mode)
              (insert ,in)
              (goto-char (point-min))
              (when (search-forward "~" nil t)
                (backward-delete-char 1)
                (set-mark (point)))
              (goto-char (point-max))
              (search-backward "|")
              (delete-char 1)
              (setq current-prefix-arg nil)
              ,@(mapcar (lambda (x)
                          (if (or (stringp x)
                                  (and (listp x)
                                       (eq (car x) 'kbd)))
                              `(evil-execute-macro 1 ,x)
                            x))
                        body)
              (insert "|")
              (when (region-active-p)
                (exchange-point-and-mark)
                ;; because not considering ~ as "on" like |
                (when (= (point) (region-end))
                  (forward-char))
                (insert "~"))
              (buffer-substring-no-properties
               (point-min)
               (point-max)))
         (and (buffer-name temp-buffer)
              (kill-buffer temp-buffer))))))

;; TODO split this up
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

(ert-deftest general-lambda-def ()
  (let ((test-map (make-sparse-keymap)))
    (general-define-key :keymaps 'test-map
      "C-c l" (lambda () (interactive) (forward-char 1)))
    (let ((def (lookup-key test-map (kbd "C-c l"))))
      ;; lambda should not be considered an extended definition
      (should (not (eq def 'lambda)))
      (should (functionp def)))))

(ert-deftest general-string-def ()
  (let ((test-map (make-sparse-keymap)))
    (general-define-key :keymaps 'test-map
      "C-c" "SPC")
    (should (string= (lookup-key test-map (kbd "C-c"))
                     (kbd "SPC")))
    (general-define-key :keymaps 'test-map
      ;; phony extended definition
      "C-c" '("SPC"))
    (should (string= (lookup-key test-map (kbd "C-c"))
                     (kbd "SPC")))))

(ert-deftest general-infix ()
  (let ((test-map (make-sparse-keymap)))
    (general-define-key :prefix "SPC"
                        :global-prefix "C-c"
                        :infix "g"
                        :keymaps 'test-map
                        :states '(normal insert)
      "a" #'git-add)
    (general-test-evil-keys 'normal test-map
      (kbd "C-c g a") #'git-add
      (kbd "SPC g a") #'git-add)
    (general-test-evil-keys 'insert test-map
      (kbd "C-c g a") #'git-add)))

(ert-deftest general-predicates ()
  (let ((test-map (make-sparse-keymap))
        (test2-map (make-sparse-keymap)))
    (general-define-key :keymaps 'test-map
                        :predicate '(looking-at "\\'")
      "<left>" test2-map
      "<right>" #'beginning-of-buffer)
    (general-define-key :keymaps 'test2-map
      "a" #'beginning-of-line)
    (should (string= (general-with "a |b c"
                       (set-transient-map test-map)
                       (kbd "<right>"))
                     "a b| c"))
    (should (string= (general-with "a b c|"
                       (set-transient-map test-map)
                       (kbd "<right>"))
                     "|a b c"))
    (should (string= (general-with "a b c|"
                       (set-transient-map test-map)
                       (kbd "<left> a"))
                     "|a b c"))
    ;; locally overriding predicate
    (general-define-key :keymaps 'test-map
                        :predicate '(not t)
      "<right>" '(beginning-of-buffer :predicate (looking-at "\\'")))
    (should (string= (general-with "a |b c"
                       (set-transient-map test-map)
                       (kbd "<right>"))
                     "a b| c"))
    (should (string= (general-with "a b c|"
                       (set-transient-map test-map)
                       (kbd "<right>"))
                     "|a b c"))))

(ert-deftest general-extended-definitions ()
  (let ((test-map (make-sparse-keymap)))
    (general-define-key :keymaps 'test-map
                        :prefix "SPC"
      "" '(nil :which-key "prefix")
      "g" '(:ignore t :which-key "git prefix"))
    (general-test-keys test-map (kbd "SPC") nil)
    ;; should not be bound
    (should (= 1 (lookup-key test-map (kbd "SPC g"))))
    (general-define-key :keymaps 'test-map
      "SPC g a" #'ccga)
    (should (keymapp (lookup-key test-map (kbd "SPC g"))))
    (should (string=
             (cdr
              (assoc "SPC"
                     which-key-key-based-description-replacement-alist))
             "prefix"))
    (should (string=
             (cdr
              (assoc "SPC g"
                     which-key-key-based-description-replacement-alist))
             "git prefix"))))

(ert-deftest general-keymap-autoload ()
  (general-define-key "C-b" nil)
  (should-error (general-define-key
                 "C-b SPC" '(:keymap general-test-map)))
  (general-define-key
   "C-b SPC" '(:keymap general-test-map :package does-not-exit))
  (should-error (execute-kbd-macro (kbd "C-b SPC")))
  (general-define-key :package 'this-has-lower-precedence
    "C-b SPC" '(:keymap general-test-map :package general-keymap-autoload-test))
  (should (not (eq (lookup-key (current-global-map) (kbd "C-b SPC"))
                   #'forward-char)))
  (should (string= (general-with "a |b c" (evil-mode -1) (kbd "C-b SPC c"))
                   "a b| c"))

  (general-test-keys (current-global-map) (kbd "C-b SPC c") #'forward-char)
  (general-define-key
   "C-b SPC" '(:keymap does-not-exis-map
               :package general-keymap-autoload-test))
  (should-error (execute-kbd-macro (kbd "C-b SPC"))))

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

(ert-deftest general-create-vim-definer ()
  (general-create-dual-vim-definer general-nmap 'normal)
  (general-nmap "~" #'tilde)
  (general-test-keys evil-normal-state-map (kbd "~") #'tilde)
  (general-test-evil-keys 'normal (current-global-map) "~" nil)
  (general-nmap "~" nil)
  (let ((general-vim-definer-default 'states))
    (general-nmap "~" #'tilde)
    (general-test-keys evil-normal-state-map (kbd "~") nil)
    (general-test-evil-keys 'normal (current-global-map) "~" #'tilde)
    (general-nmap "~" nil))
  (general-create-dual-vim-definer general-nmap 'normal t)
  (general-nmap "~" #'tilde)
  (general-test-keys evil-normal-state-map (kbd "~") nil)
  (general-test-evil-keys 'normal (current-global-map) "~" #'tilde)
  (general-create-dual-vim-definer general-iemap '(insert emacs))
  (general-iemap "C-b" 'c-b)
  (general-test-keys evil-insert-state-map (kbd "C-b") 'c-b)
  (general-test-keys evil-emacs-state-map (kbd "C-b") 'c-b))

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

(ert-deftest general-key-dispatch ()
  (general-emacs-define-key evil-normal-state-map
    "c" (general-key-dispatch #'evil-change
          "ow" #'evil-forward-word-begin
          ;; to be more obvious
          "w" (general-simulate-keys (#'evil-change "3iw"))
          ;; either works
          ;; "c" #'evil-change-whole-line
          "c" (general-simulate-keys (#'evil-change "c"))))
  ;; basic tests
  (should (string= (general-with "|one two" "cow")
                   "one |two"))
  (should (string= (general-with "|one two three" "cw")
                   "| three"))
  (should (string= (general-with "|a\na\na" "cc")
                   "|\na\na"))
  (should (string= (general-with "one |two" "cb")
                   "|two"))
  ;; test evil-want-change-word-to-end
  (let ((evil-want-change-word-to-end t))
    (should (string= (general-with "|one two" "cW")
                     "| two")))
  (let ((evil-want-change-word-to-end nil))
    (should (string= (general-with "|one two" "cW")
                     "|two"))))

(ert-deftest general-dispatch-repeating ()
  (general-emacs-define-key evil-normal-state-map
    "c" (general-key-dispatch #'evil-change
          "ow" #'evil-forward-word-begin
          ;; to be more obvious
          "w" (general-simulate-keys (#'evil-change "3iw"))
          ;; either works
          ;; "c" #'evil-change-whole-line
          "c" (general-simulate-keys (#'evil-change "c"))))
  ;; should not repeat for commands that should not repeat
  (setq evil-repeat-ring (make-ring 10))
  (should (string= (general-with "|one two three"
                     (kbd "c i w ESC c o w ."))
                   "|  three"))
  ;; motions in insert state should repeat
  (let ((evil-move-cursor-back nil))
    (should (string= (general-with "|one two three"
                       (kbd "i C-o c o w ESC ."))
                     "one two |three")))
  ;; if declare it repeatable, should work in normal state too
  (should (string= (general-with "|one two three"
                     (evil-declare-repeat #'evil-forward-word-begin)
                     "cow.")
                   "one two |three"))
  (let ((evil-want-change-word-to-end nil))
    (should (string= (general-with "|one two three four" (kbd "c W ESC ."))
                     "|three four")))
  (should (string= (general-with "|one two three" (kbd "c a w ESC ."))
                   "|three"))
  (should (string= (general-with "|one two three four" (kbd "c w ESC ."))
                   "|four"))
  (should (string= (general-with "|a\nb\nc\nd" (kbd "c c ESC j ."))
                   "\n|\nc\nd")))

(ert-deftest general-dispatch-counts ()
  (general-emacs-define-key evil-normal-state-map
    "c" (general-key-dispatch #'evil-change
          "ow" #'evil-forward-word-begin
          ;; to be more obvious
          "w" (general-simulate-keys (#'evil-change "3iw"))
          ;; either works
          ;; "c" #'evil-change-whole-line
          "c" (general-simulate-keys (#'evil-change "c"))))
  (should (string= (general-with "|one two three" "2cow")
                   "one two |three"))
  (should (string= (general-with "|one two three four" "2cw")
                   "|four"))
  (should (string= (general-with "|a\na\na" "2cc")
                   "|\na"))
  (should (string= (general-with "|a\na\na" "c2c")
                   "|\na"))
  (should (string= (general-with "one two |three" "2cb")
                   "|three"))
  (should (string= (general-with "one two |three" "c2b")
                   "|three")))

(ert-deftest general-dispatch-repeating-and-counts ()
  (general-emacs-define-key evil-normal-state-map
    "c" (general-key-dispatch #'evil-change))
  (let ((evil-move-cursor-back nil)
        (evil-want-change-word-to-end nil))
    (should (string= (general-with "|one two three four five six"
                                   (kbd "c 2 W ESC ."))
                     "|five six"))
    (should (string= (general-with "|a b c d e f g h i j k l m n o p q r s t u"
                                   (kbd "c 1 0 W ESC ."))
                     "|u"))))

(ert-deftest general-dispatch-jk ()
  (general-emacs-define-key evil-insert-state-map
    "j" (general-key-dispatch #'self-insert-command
          "k" #'evil-normal-state
          "w" (lambda () (interactive) (insert-string "word"))))
  (should (string= (general-with "|" "ijki")
                   "|"))
  (should (string= (general-with "|" "ijw")
                   "word|"))
  (should (string= (general-with "|" "iji")
                   "ji|"))
  (should (string= (general-with "|" "ijogging")
                   "jogging|")))

(ert-deftest general-dispatch-seq-counts ()
  ;; test that properly handles case where bound to a key sequence > length 1
  (general-emacs-define-key evil-normal-state-map
    "z" nil
    "zbg" (general-key-dispatch #'evil-change
            "c" #'evil-change-whole-line
            "ttzyx" #'evil-forward-word-begin))
  (should (string= (general-with "|a\na\na" "2zbgc")
                   "|\na"))
  (should (string= (general-with "|one two three" "2zbgttzyx")
                   "one two |three"))
  (let ((evil-want-change-word-to-end nil))
    (should (string= (general-with "|one two three" "2zbgw")
                     "|three"))))

(ert-deftest general-dispatch-seq-repeating ()
  ;; test that properly handles case where bound to a key sequence > length 1
  (general-emacs-define-key evil-normal-state-map
    "z" nil
    "zbg" (general-key-dispatch #'evil-change
            "c" #'evil-change-whole-line
            "ttzyx" #'evil-forward-word-begin))
  (evil-declare-repeat #'evil-forward-word-begin)
  ;; TODO this gives "Variable binding depth exceeds max-specpdl-size"
  ;; (let ((evil-want-change-word-to-end nil))
  ;;   (should (string= (general-with "|one two three" (kbd "z b g w ESC ."))
  ;;                    "|three")))
  (should (string= (general-with "|one\ntwo\nthree" (kbd "z b g c ESC j ."))
                   "\n|\nthree"))
  (should (string= (general-with "|one two three" "zbgttzyx.")
                   "one two |three"))
  (should (string= (general-with "|one two three four" "zbgttzyx..")
                   "one two three |four")))

(ert-deftest general-dispatch-operator ()
  (general-emacs-define-key evil-normal-state-map
    "d" (general-key-dispatch #'evil-delete
          "d" #'evil-delete))
  (should (string= (general-with "|a b c" "ddaw.")
                   "|c")))

(ert-deftest general-predicate-dispatch ()
  ;; TODO undo keybinding changes
  (general-define-key "<right>"
                      (general-predicate-dispatch 'right-char
                        :docstring "Move right or to the bol."
                        (eolp) 'beginning-of-line))
  (should (string= (general-with "|a b"
                     (evil-mode -1)
                     (kbd "<right>"))
                   "a| b"))
  (should (string= (general-with "a b|"
                     (evil-mode -1)
                     (kbd "<right>"))
                   "|a b")))

(ert-deftest general-use-package-keyword ()
  (let ((general-test-map (make-sparse-keymap))
        (test-key "k")
        (test-command #'k))
    (use-package evil
      :general
      (:keymaps 'general-test-map
       "j" #'j)
      ("a" "b" :keymaps 'general-test-map)
      (test-key test-command :keymaps 'general-test-map)
      (general-emacs-define-key general-test-map "l" #'l)
      (general-evil-define-key 'normal general-test-map "m" #'m))
    (should (string= (lookup-key general-test-map "a") "b"))
    (general-test-keys general-test-map
      "j" #'j
      "k" #'k
      "l" #'l)
    (general-test-evil-keys 'normal general-test-map "m" #'m))
  (use-package general-keymap-autoload-2-test
    :general ("C-b" '(:keymap general-test2-map)))
  (should (functionp (lookup-key (current-global-map) (kbd "C-b"))))
  (should (string= (general-with "a |b c"
                     (evil-mode -1)
                     ;; doesn't work
                     ;; (set-transient-map general-test-map)
                     (kbd "C-b z"))
                   "a b| c"))
  (general-test-keys (current-global-map) (kbd "C-b z") #'forward-char))

(provide 'general-tests)
;;; general-tests.el ends here
