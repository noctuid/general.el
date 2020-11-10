;;; test-general.el --- Tests for general.el. -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Tests for general.el

;; Conventions:
;; - `general-temp-map' is used for defs that will be tested by lookup
;; - `general-test-mode-map' is used for defs whose behavior will be tested
;; - `general-temp-map' should be empty at the beginning/end of each spec; it
;;   should be guaranteed that any lookup initially returns nil
;; - `general-test-mode-map' should never be set to (make-sparse-keymap); doing
;;   this breaks a lot tests for no clear reason (evil-normalize-keymaps does
;;   not help); therefore, tests should not rely on `general-test-mode-map'
;;   being empty
;; - global keymaps such as `evil-normal-state-map' should never be altered;
;;   they should be let bound when key definitions take place or the key
;;   definitions should be reverted
;; - `general-define-key' args should appear starting on the next line and in
;;   the order that they appear in the definition
;; - Only `evil-local-mode' should be turned on/off within `general-with' (so
;;   the global value isn't affected when testing in Emacs directly)

;; TODO
;; - way to set the current global map
;; - way to wrap all specs with let automatically
;; - test :major-modes (and with aliases; should work now but previously didn't)
;; - test :definer
;; - test rest of which-key keywords
;; - test 'global
;; - test all local keywords in extended definitions
;; - test all def types in extended definitions
;; - implement other xits
;; - implement other todos

;;; Code:
;; * Setup
(when (require 'undercover nil t)
  (undercover "*.el"
              (:exclude "test-*.el")
              (:report-type :codecov)
              (:send-report nil)))

(require 'buttercup)
(require 'general)
(require 'evil)
(require 'which-key)
(require 'use-package)

(push "tests/" load-path)

(setq evil-want-change-word-to-end nil
      evil-move-cursor-back nil
      evil-move-beyond-eol t)

(defvar general-test-mode-map (make-sparse-keymap))

(define-minor-mode general-test-mode
  "A minor mode for general.el tests."
  :lighter ""
  :keymap general-test-mode-map
  (evil-normalize-keymaps))

(defvar general-temp-map (make-sparse-keymap))
(push '(temp . general-temp-map) general-keymap-aliases)

;; * Testing Helpers
(defmacro general-with (in &rest body)
  "This is `lispy-with' modified for general.
Note that | is considered to be \"on\" a character, meaning that it is included
in a visual selection. ~ on the other hand is not considered to be on a
character, so when it represents the region end, the character after it is not
considered as part of the region."
  (declare (indent 1))
  `(let ((temp-buffer (generate-new-buffer " *temp*"))
         (general--simulate-next-as-is t))
     (save-window-excursion
       (unwind-protect
           (progn
             (switch-to-buffer temp-buffer)
             (emacs-lisp-mode)
             (general-test-mode)
             (transient-mark-mode)
             (evil-local-mode)
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
                         (if (stringp x)
                             `(evil-execute-macro 1 (kbd ,x))
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

(defun general-test-keys (states keymaps &rest maps)
  "Look in the keymaps for STATES and KEYMAPS for MAPS.
Return t if successful or a cons corresponding to the failed key and def."
  (declare (indent 2))
  (unless (listp states)
    (setq states (list states)))
  (unless (and (listp keymaps)
               (not (keymapp keymaps)))
    (setq keymaps (list keymaps)))
  (let (actual-keymaps)
    (if states
        (dolist (state states)
          (dolist (keymap keymaps)
            (if state
                (push (evil-get-auxiliary-keymap keymap state t)
                      actual-keymaps)
              (setq actual-keymaps (append actual-keymaps keymaps)))))
      (setq actual-keymaps keymaps))
    (dolist (keymap actual-keymaps)
      (while maps
        (let* ((key (kbd (pop maps)))
               (def (pop maps))
               (actual-def (lookup-key keymap key)))
          (unless (equal actual-def
                         (if (stringp def)
                             (kbd def)
                           def))
            (buttercup-fail "Key \"%s\" is not bound to \"%s\" but to \"%s\""
                            (key-description key)
                            def
                            actual-def))))))
  t)

;; * Key Definition
;; ** Main Definer
(describe "general-define-key"
  ;; NOTE: this applies to all specs even when nesting describes
  (after-each
    (setq general-temp-map (make-sparse-keymap)))
  (it "should define/undefine keys in (current-global-map) by default"
    (general-define-key "a" #'a)
    (expect (general-test-keys nil (current-global-map)
              "a" #'a))
    (general-define-key "a" nil)
    (expect (general-test-keys nil (current-global-map)
              "a" nil))
    (general-define-key "a" #'self-insert-command))
  (it "should allow defining/undefining keys in a specified keymap"
    (general-define-key
     :keymaps 'general-temp-map
     "a" #'a)
    (expect (general-test-keys nil general-temp-map
              "a" #'a))
    (general-define-key
     :keymaps 'general-temp-map
     "a" nil)
    (expect (general-test-keys nil general-temp-map
              "a" nil)))
  (it "should allow defining/undefining keys in a specified state and keymap"
    (general-define-key
     :states 'normal
     :keymaps 'general-temp-map
     "a" #'a)
    (expect (general-test-keys 'normal general-temp-map
              "a" #'a))
    (general-define-key
     :keymaps 'general-temp-map
     :states 'normal
     "a" nil)
    (expect (general-test-keys 'normal general-temp-map
              "a" nil)))
  (it "should use the evil global keymaps for :keymaps 'global :states ..."
    (let ((evil-normal-state-map (make-sparse-keymap)))
      (general-define-key
       :states 'normal
       "a" 'a)
      (expect (general-test-keys 'normal (current-global-map)
                "a" nil))
      (expect (general-test-keys nil evil-normal-state-map
                "a" #'a))))
  (xit "should allow defining/undefining keys in multiple states and keymaps")
  (it "should support keymap/state aliasing"
    (let ((evil-normal-state-map (make-sparse-keymap)))
      (general-define-key
       :keymaps 'normal
       "a" #'a)
      (general-define-key
       :keymaps 'n
       "b" #'b)
      (expect (general-test-keys nil evil-normal-state-map
                "a" #'a
                "b" #'b)))
    (general-define-key
     :states 'n
     :keymaps 'general-temp-map
     "a" #'a)
    (expect (general-test-keys 'normal general-temp-map
              "a" #'a)))
  (it "should allow defining local keybindings"
    ;; Note: local is not a keymap alias (it is special and turns on
    ;; `general-override-local-mode')
    (expect (general-with "fo|o"
              (evil-local-mode -1)
              (general-define-key
               :keymaps 'local
               "C-a" #'backward-char)
              "C-a")
            :to-equal "f|oo")
    ;; should not affect other buffers
    (expect (general-with "fo|o"
              (evil-local-mode -1)
              (general-override-local-mode)
              "C-a")
            :to-equal "|foo")
    (general-define-key
     :states 'normal
     :keymaps 'general-test-mode-map
     "a" #'forward-char)
    (expect (general-with "fo|o"
              (general-define-key
               :states 'normal
               :keymaps 'local
               "a" #'backward-char)
              "a")
            :to-equal "f|oo"))
  (describe "should support all valid define-key defs"
    ;; NOTE: See key lookup in the manual and the help for define-key
    (it "including nil (tested previously)")
    (it "including symbols/commands (tested previously)")
    (it "including strings and vectors (arrays)"
      (general-define-key
       :keymaps 'general-temp-map
       "a" "a")
      (expect (general-test-keys nil general-temp-map
                "a" "a"))
      (general-define-key
       :keymaps 'general-temp-map
       "test" [116 101 115 116])
      (expect (general-test-keys nil general-temp-map
                "test" [116 101 115 116])))
    (it "including keymaps (list)"
      (general-define-key
       :keymaps 'general-temp-map
       "t" general-temp-map)
      (expect (general-test-keys nil general-temp-map
                "t" general-temp-map)))
    (it "including interactive lambdas (list)"
      (general-define-key
       :keymaps 'general-temp-map
       "l" (lambda () (interactive) (forward-char 1)))
      (expect (general-test-keys nil general-temp-map
                "l" (lambda () (interactive) (forward-char 1)))))
    (it "including extended menu items (list)"
      (general-define-key
       :keymaps 'general-temp-map
       "m" '(menu-item "" nil
                       :filter (lambda (&optional _)
                                 (when t
                                   'next-line))))
      (expect (general-test-keys nil general-temp-map
                "m" #'next-line)))
    (xit "including conses of the form (STRING . DEFN)")
    (xit "including conses of the form (MAP . CHAR)"))
  (it "should automatically wrap keys with (kbd) (and work for vector keys)"
    (general-define-key
     :keymaps 'general-temp-map
     "SPC a" #'spc-a
     [remap kill-line] #'my-kill-line
     [24 108] #'C-x--l
     [?\C-x ?a] #'C-x--a)
    (expect (general-test-keys nil general-temp-map
              "SPC a" #'spc-a
              "<remap> <kill-line>" #'my-kill-line
              "C-x l" #'C-x--l
              "C-x a" #'C-x--a)))
  (it "should automatically wrap string definitions with (kbd)"
    (general-define-key
     :keymaps 'general-temp-map
     "SPC b" "C-x b")
    (expect (general-test-keys nil general-temp-map
              "SPC b" "C-x b")))
  (describe "should support prefixes"
    (it "in the basic case"
      (general-define-key
       :keymaps 'general-temp-map
       :prefix ","
       "a" #'comma-a)
      (general-define-key
       :states '(normal insert)
       :keymaps 'general-temp-map
       :prefix ","
       "a" #'comma-a)
      (expect (general-test-keys '(insert normal nil) general-temp-map
                ",a" #'comma-a)))
    (it "and binding them with \"\""
      (general-define-key
       :keymaps 'general-temp-map
       "," #'bound)
      (general-define-key
       :keymaps 'general-temp-map
       :prefix ","
       ;; unbind first
       "" nil
       "a" #'comma-a)
      (expect (general-test-keys nil general-temp-map
                ",a" #'comma-a)))
    (describe "and creating"
      (it "prefix commands and keymaps"
        (general-define-key
         :keymaps 'general-temp-map
         :prefix ","
         :prefix-command 'my-comma-prefix
         :prefix-map 'my-comma-prefix-map
         :prefix-name "my comma prefix map"
         "b" #'comma-b)
        ;; the prefix should be bound to the prefix keymap
        (general-define-key
         :keymaps 'my-comma-prefix-map
         "c" #'comma-c)
        (expect (fboundp 'my-comma-prefix))
        (expect (and (boundp 'my-comma-prefix-map)
                     (keymapp my-comma-prefix-map)))
        (expect (keymap-prompt my-comma-prefix-map)
                :to-equal "my comma prefix map")
        ;; previously created keymaps should not be cleared
        (general-define-key
         :keymaps 'general-temp-map
         :prefix ","
         :prefix-command 'my-comma-prefix
         :prefix-map 'my-comma-prefix-map
         :prefix-name "my comma prefix map")
        (expect (general-test-keys nil general-temp-map
                  ",b" #'comma-b
                  ",c" #'comma-c))
        ;; cleanup
        (fmakunbound 'my-comma-prefix)
        (makunbound 'my-comma-prefix-map)
        (expect (not (or (boundp 'my-comma-prefix-map)
                         (fboundp 'my-comma-prefix)))))
      (it "just prefix keymaps"
        (general-define-key
         :keymaps 'general-temp-map
         :prefix ","
         :prefix-map 'my-comma-prefix-map
         :prefix-name "my comma prefix map"
         "b" #'comma-b)
        (expect (not (fboundp 'my-comma-prefix)))
        (expect (and (boundp 'my-comma-prefix-map)
                     (keymapp my-comma-prefix-map)))
        (expect (keymap-prompt my-comma-prefix-map)
                :to-equal "my comma prefix map")
        ;; the prefix should be bound to the prefix keymap
        (general-define-key
         :keymaps 'my-comma-prefix-map
         "c" #'comma-c)
        ;; previously created keymaps should not be cleared
        (general-define-key
         :keymaps 'general-temp-map
         :prefix ","
         :prefix-map 'my-comma-prefix-map
         :prefix-name "my comma prefix map")
        (expect (general-test-keys nil general-temp-map
                  ",b" #'comma-b
                  ",c" #'comma-c))
        ;; cleanup
        (makunbound 'my-comma-prefix-map)
        (expect (not (boundp 'my-comma-prefix-map)))))
    (it "with a vector key and/or vector prefix"
      (general-define-key
       :keymaps 'general-temp-map
       :prefix ","
       [?c] #'comma-c)
      (general-define-key
       :prefix [?,]
       :keymaps 'general-temp-map
       "d" #'comma-d)
      (general-define-key
       :prefix [?,]
       :keymaps 'general-temp-map
       [?e] #'comma-e)
      (expect (general-test-keys nil general-temp-map
                ",c" #'comma-c
                ",d" #'comma-d
                ",e" #'comma-e)))
    (it "with :non-normal-prefix, :global-prefix, and :infix"
      (general-define-key
       :states '(normal insert emacs)
       :keymaps 'general-temp-map
       :prefix ","
       :non-normal-prefix "M-,"
       :global-prefix "C-,"
       :infix "f"
       "g" #'comma-f-g)
      (expect (general-test-keys 'normal general-temp-map
                "M-," nil
                "C-, f g" #'comma-f-g
                ",fg" #'comma-f-g))
      (expect (general-test-keys '(insert emacs) general-temp-map
                "," nil
                "C-, f g" #'comma-f-g
                "M-, f g" #'comma-f-g))
      ;; case where non-normal maps but no non-normal-prefix
      (general-define-key
       :states 'insert
       :keymaps 'general-temp-map
       :global-prefix "C-,"
       "a" #'comma-a)
      (expect (general-test-keys 'insert general-temp-map
                "C-, a" #'comma-a))
      (let ((evil-normal-state-map (make-sparse-keymap))
            (evil-insert-state-map (make-sparse-keymap)))
        ;; TODO with just prefix and global and just prefix and non-normal
        (general-define-key
         :keymaps '(evil-normal-state-map
                    evil-insert-state-map)
         :prefix ","
         :non-normal-prefix "M-,"
         :global-prefix "C-,"
         :infix "f"
         "g" #'comma-f-g)
        (expect (general-test-keys nil evil-normal-state-map
                  "M-," nil
                  "C-, f g" #'comma-f-g
                  ",fg" #'comma-f-g))
        (expect (general-test-keys nil evil-insert-state-map
                  "," nil
                  "C-, f g" #'comma-f-g
                  "M-, f g" #'comma-f-g)))))
  (it "should support creating and binding in prefix keymaps with no prefix"
    (general-define-key
     :prefix-command 'general-unbound-prefix-map
     "a" #'a)
    (expect (general-test-keys nil general-unbound-prefix-map
              "a" #'a))
    (general-define-key
     :prefix-map 'general-unbound-prefix2-map
     "a" #'a)
    (expect (general-test-keys nil general-unbound-prefix2-map
              "a" #'a))
    ;; cleanup
    (makunbound 'general-unbound-prefix-map)
    (makunbound 'general-unbound-prefix2-map)
    (expect (not (boundp 'general-unbound-prefix-map)))
    (expect (not (boundp 'general-unbound-prefix2-map))))
  (it "should support predicates"
    (general-define-key
     :keymaps 'general-temp-map
     "a" #'beginning-of-line)
    (general-define-key
     :states 'normal
     :keymaps 'general-test-mode-map
     :predicate '(looking-at "\\'")
     "<left>" general-temp-map
     "<right>" #'beginning-of-buffer)
    (expect (general-with "a |b c" "<right>")
            :to-equal "a b| c")
    (expect (general-with "a b c|" "<right>")
            :to-equal "|a b c")
    (expect (general-with "a |b c" "<left>")
            :to-equal "a| b c")
    (expect (general-with "a b c|" "<left> a")
            :to-equal "|a b c"))
  (it "should support local predicates"
    (general-define-key
     :states 'normal
     :keymaps 'general-test-mode-map
     :predicate '(not t)
     "<right>" '(beginning-of-buffer :predicate (looking-at "\\'")))
    (expect (general-with "a |b c" "<right>")
            :to-equal "a b| c")
    (expect (general-with "a b c|" "<right>")
            :to-equal "|a b c"))
  (describe "should support extended definitions"
    (it "including support for :ignore, which should not bind the key"
      (general-define-key
       :keymaps 'general-temp-map
       "a" '(:ignore))
      (expect (general-test-keys nil general-temp-map
                "a" nil)))
    (it "including support for :prefix-command, :prefix-map, and :prefix-name"
      (general-define-key
       :keymaps 'general-temp-map
       "a" '(:prefix-command a-command :prefix-map a-map :prefix-name "a map")
       "ab" #'a-b)
      (expect (fboundp 'a-command))
      (expect (and (boundp 'a-map)
                   (keymapp a-map)))
      (expect (keymap-prompt a-map)
              :to-equal "a map")
      (expect (general-test-keys nil general-temp-map
                "ab" #'a-b)))
    (it "including support for \"autoloading\" keymaps with :keymap"
      ;; TODO consider splitting this into multiple specs
      ;; should error without :package
      (expect (general-define-key
               :keymaps 'general-temp-map
               "," '(:keymap general-autoload-map))
              :to-throw 'error)
      (general-define-key
       :states 'normal
       :keymaps 'general-test-mode-map
       "," '(:keymap general-autoload-map :package does-not-exist))
      ;; should error when non-existent package is specified
      (expect (general-with ",") :to-throw)
      ;; package properly specified
      (general-define-key
       :states 'normal
       :keymaps 'general-test-mode-map
       :package 'this-has-lower-precedence
       "," '(:keymap general-autoload-map
             :package general-keymap-autoload)
       "SPC" '(:keymap does-not-exist-map
               :package general-keymap-autoload))
      ;; not yet bound
      (expect (general-test-keys 'normal general-test-mode-map
                ",f" #'forward-char)
              :to-throw)
      (expect (general-with "a |b c" ",f")
              :to-equal "a b| c")
      (expect (general-test-keys 'normal general-test-mode-map
                ",f" #'forward-char))
      ;; should error if keymap doesn't exist in package
      (expect (general--simulate-keys nil "SPC" 'normal general-test-mode-map)
              :to-throw))
    (it "including support for :which-key"
      (defvar general-command-map (make-sparse-keymap))
      (setq which-key-replacement-alist nil)
      (general-define-key
       :keymaps 'general-temp-map
       :prefix ","
       ;; basic replacement
       "" '(:ignore t :which-key "general prefix")
       "f" '(:ignore t :which-key "file prefix")
       ;; should be quoted
       "[" '(:ignore t :which-key "open square bracket")
       ;; use a cons as a replacement
       "g" '(:ignore t :wk ("g-key" . "git prefix"))
       ;; toggle lispy; use a function as a replacement to show if currently on
       "l" '(lispy-mode :wk my-lispy-which-key-display)
       ;; for a keymap, only the keys will be matched;
       ;; :no-match-binding is not necessary
       "G" '(:keymap general-command-map :wk "general prefix")
       "z" '(no-display-command :wk t))
      (general-define-key
       :keymaps 'general-temp-map
       :wk-full-keys nil
       "A" '(:prefix-command apropos-prefix-map :which-key "apropos"))
      ;; should behave the same as previous
      (general-define-key
       :keymaps 'general-temp-map
       :prefix "A"
       :prefix-command 'apropos-prefix-map
       :wk-full-keys nil
       "" '(:ignore t :which-key "new apropos description"))
      (expect which-key-replacement-alist
              :to-equal '((("A\\'" . "apropos-prefix-map")
                           nil . "new apropos description")
                          (("A\\'" . "apropos-prefix-map")
                           nil . "apropos")
                          (("\\`, z\\'" . "no-display-command")
                           . t)
                          (("\\`, G\\'")
                           nil . "general prefix")
                          (("\\`, l\\'" . "lispy-mode")
                           . my-lispy-which-key-display)
                          (("\\`, g\\'")
                           "g-key" . "git prefix")
                          (("\\`, \\[\\'")
                           nil . "open square bracket")
                          (("\\`, f\\'")
                           nil . "file prefix")
                          (("\\`,\\'")
                           nil . "general prefix"))))
    (describe "including support for :properties, :repeat, and :jump"
      (it "globally"
        (general-define-key
         :keymaps 'general-temp-map
         :properties '(:repeat t :jump t)
         "a" 'general-should-repeat-and-jump)
        (expect (evil-get-command-properties 'general-should-repeat-and-jump)
                :to-equal '(:repeat t :jump t))
        (evil-set-command-properties 'general-should-repeat-and-jump)
        (expect (evil-get-command-properties 'general-should-repeat-and-jump)
                :to-be nil))
      (it "locally"
        (general-define-key
         :keymaps 'general-temp-map
         "a" '(general-should-repeat-and-jump :properties (:repeat t :jump t)))
        (expect (evil-get-command-properties 'general-should-repeat-and-jump)
                :to-equal '(:repeat t :jump t))
        (evil-set-command-properties 'general-should-repeat-and-jump)
        (expect (evil-get-command-properties 'general-should-repeat-and-jump)
                :to-be nil)
        (general-define-key
         :keymaps 'general-temp-map
         "a" '(general-should-repeat-and-jump :repeat t :jump t))
        (expect (evil-get-command-properties
                 'general-should-repeat-and-jump)
                :to-equal '(:repeat t :jump t))
        (evil-set-command-properties 'general-should-repeat-and-jump)
        (expect (evil-get-command-properties 'general-should-repeat-and-jump)
                :to-be nil))
      (it "locally with a global default"
        (general-define-key
         :keymaps 'general-temp-map
         :properties '(:repeat nil :jump nil)
         "a" '(general-should-repeat-and-jump :properties (:repeat t :jump t)))
        (expect (evil-get-command-properties 'general-should-repeat-and-jump)
                :to-equal '(:repeat t :jump t))
        (evil-set-command-properties 'general-should-repeat-and-jump)
        (expect (evil-get-command-properties 'general-should-repeat-and-jump)
                :to-be nil)
        (general-define-key
         :keymaps 'general-temp-map
         :repeat nil
         :jump nil
         "a" '(general-should-repeat-and-jump :repeat t :jump t))
        (expect (evil-get-command-properties
                 'general-should-repeat-and-jump)
                :to-equal '(:repeat t :jump t))
        (evil-set-command-properties 'general-should-repeat-and-jump)
        (expect (evil-get-command-properties 'general-should-repeat-and-jump)
                :to-be nil))))
  (it "should support delaying keybindings until the keymap exists"
    (general-define-key
     :keymaps 'general-delay-map
     "a" #'a)
    (expect (not (boundp 'general-delay-map)))
    (require 'general-delay)
    (expect (general-test-keys nil general-delay-map
              "a" #'a)))
  (it "should support delaying keybindings until the state exists"
    (general-define-key
     :states 'non-existent
     "a" #'a)
    (expect (not (boundp 'evil-non-existent-state-map)))
    (require 'general-non-existent-state)
    (expect (general-test-keys nil evil-non-existent-state-map
              "a" #'a))))

;; ** Positional Definers
(describe "general-emacs-define-key"
  (after-each
    (setq general-temp-map (make-sparse-keymap)))
  (it "should take a positional argument for the keymap"
    (general-emacs-define-key general-temp-map
      "a" #'a)
    (expect (general-test-keys nil general-temp-map
              "a" #'a))
    ;; quoting is fine as well
    (general-emacs-define-key 'general-temp-map
      "b" #'b)
    (expect (general-test-keys nil general-temp-map
              "b" #'b))
    (general-emacs-define-key (general-temp-map
                               general-test-mode-map)
      "c" #'c)
    (expect (general-test-keys nil (list general-temp-map
                                         general-test-mode-map)
              "c" #'c))
    (general-emacs-define-key '(general-temp-map
                                general-test-mode-map)
      "d" #'d)
    (expect (general-test-keys nil (list general-temp-map
                                         general-test-mode-map)
              "d" #'d))))

(describe "general-evil-define-key"
  (after-each
    (setq general-temp-map (make-sparse-keymap)))
  (it "should take two positional arguments for the state and keymap"
    (general-evil-define-key normal general-temp-map
      "a" #'a)
    (expect (general-test-keys 'normal general-temp-map
              "a" #'a))
    ;; quoting is fine as well
    (general-evil-define-key 'normal general-temp-map
      "b" #'b)
    (expect (general-test-keys 'normal general-temp-map
              "b" #'b))
    (general-evil-define-key (normal insert) general-temp-map
      "c" #'c)
    (expect (general-test-keys '(normal insert) general-temp-map
              "c" #'c))
    (general-evil-define-key '(normal insert) general-temp-map
      "d" #'d)
    (expect (general-test-keys '(normal insert) general-temp-map
              "d" #'d))))

(describe "general-def"
  (after-each
    (setq general-temp-map (make-sparse-keymap)))
  (it "should act as `general-define-key' when given 0 positional args"
    (general-def :keymaps 'general-temp-map
      "a" #'a)
    (expect (general-test-keys nil general-temp-map
              "a" #'a)))
  (it "should act as `general-emacs-define-key' when given 1 positional arg"
    (general-def general-temp-map
      "a" #'a)
    (expect (general-test-keys nil general-temp-map
              "a" #'a)))
  (it "should act as `general-evil-define-key' when given 2 positional args"
    (general-def 'normal general-temp-map
      "a" #'a)
    (expect (general-test-keys 'normal general-temp-map
              "a" #'a))))

(describe "general-defs"
  (after-each
    (setq general-temp-map (make-sparse-keymap)))
  (it "should split into multiple `general-def's"
    (general-defs
      general-temp-map
      :states 'normal
      "a" #'a
      [?b] #'b
      'visual general-temp-map
      "c" #'c
      :states 'insert :keymaps 'general-temp-map
      "d" #'d)
    (expect (general-test-keys 'normal general-temp-map
              "a" #'a
              "b" #'b))
    (expect (general-test-keys 'visual general-temp-map
              "c" #'c))
    (expect (general-test-keys 'insert general-temp-map
              "d" #'d))))

;; ** Unbind Wrapper
(describe "general-unbind"
  (after-each
    (setq general-temp-map (make-sparse-keymap)))
  (it "should allow unbinding/ignoring keys en masse"
    (general-define-key
     :keymaps 'general-temp-map
     "a" #'a
     "b" #'b
     "c" #'c)
    (general-unbind
      "a"
      "b"
      "c"
      ;; keywords should work at odd positions (e.g. keywords are often added at
      ;; end as defaults)
      :keymaps 'general-temp-map)
    (expect (general-test-keys nil general-temp-map
              "a" nil
              "b" nil
              "c" nil))
    (general-unbind
      :with #'ignore
      :keymaps 'general-temp-map
      "a"
      "b"
      "c")
    (expect (general-test-keys nil general-temp-map
              "a" #'ignore
              "b" #'ignore
              "c" #'ignore)))
  (it "should allow positional arguments (wraps general-def)"
    (general-define-key
     :keymaps 'general-temp-map
     "a" #'a
     "b" #'b
     "c" #'c)
    (general-unbind general-temp-map
      "a"
      "b"
      "c")
    (expect (general-test-keys nil general-temp-map
              "a" nil
              "b" nil
              "c" nil))
    ;; :with keyword should work at an odd position (must be handled internally)
    (general-unbind 'general-temp-map
      :with #'ignore
      "a"
      "b"
      "c")
    (expect (general-test-keys nil general-temp-map
              "a" #'ignore
              "b" #'ignore
              "c" #'ignore))))

;; ** User-created Definers
(describe "wrappers created with `general-create-definer'"
  (before-all
    (general-create-definer general-nmap :states 'normal)
    (general-create-definer general-emacs-def
      :wrapping general-emacs-define-key
      :states 'normal))
  (after-each
    (setq general-temp-map (make-sparse-keymap)))
  (it "should use the specified arguments by default"
    (let ((evil-normal-state-map (make-sparse-keymap)))
      (general-nmap "a" 'a)
      (expect (general-test-keys nil evil-normal-state-map
                "a" #'a)))
    (general-nmap
      :keymaps 'general-temp-map
      "a" #'a)
    (expect (general-test-keys 'normal general-temp-map
              "a" #'a)))
  (it "should allow positional arguments (general-def)"
    (general-nmap general-temp-map
      "a" #'a)
    (expect (general-test-keys 'normal general-temp-map
              "a" #'a)))
  (it "should allow overriding the default arguments"
    (general-nmap
      :states 'visual
      :keymaps 'general-temp-map
      "a" #'a)
    (expect (general-test-keys 'normal general-temp-map
              "a" nil))
    (expect (general-test-keys 'visual general-temp-map
              "a" #'a)))
  (it "should allow wrapping any definer"
    (let ((key "a"))
      (general-emacs-def general-temp-map key #'a)
      (general-emacs-def general-temp-map
        key #'a
        :states nil))
    (expect (general-test-keys 'normal general-temp-map
              "a" #'a))
    (expect (general-test-keys nil general-temp-map
              "a" #'a))))

;; ** Use-package Keywords
;; *** :general
(describe "the :general use-package keyword"
  (before-all
    (general-create-definer general-nmap
      :states 'normal))
  (after-each
    (setq general-temp-map (make-sparse-keymap)))
  (it "should work as general-def by default"
    (use-package some-package
      :ensure nil
      :general
      ;; general-define-key
      (:keymaps 'general-temp-map
       "a" #'a)
      ("b" "b" :keymaps 'general-temp-map)
      ("c" #'c :keymaps 'general-temp-map)
      ;; general-emacs-define-key
      (general-temp-map "d" #'d)
      ;; general-evil-define-key
      ('normal general-temp-map "e" #'e))
    (expect (general-test-keys nil general-temp-map
              "a" #'a
              "b" "b"
              "c" #'c
              "d" #'d))
    (expect (general-test-keys 'normal general-temp-map
              "e" #'e)))
  (xit "should support any definer when it is manually specified"
    (expect (and (fboundp 'general-nmap)
                 (macrop 'general-nmap)))
    (use-package some-package
      :ensure nil
      :general
      (general-nmap general-temp-map "a" #'a))
    ;; TODO macro expansion is correct and this works in emacs, but it does not
    ;; work here fo some reason
    ;; (message (format "%s"
    ;;                  (macroexpand '(use-package some-package
    ;;                                  :ensure nil
    ;;                                  :general
    ;;                                  (general-nmap general-temp-map "a" #'a)))))
    (expect (general-test-keys 'normal general-temp-map
              "a" #'a)))
  (it "should correctly extract definitions from general definer arglists"
    (expect (plist-get (use-package-normalize/:general
                        nil
                        nil
                        '((general-def "key" #'command)))
                       :commands)
            :to-equal '(command))
    (expect (plist-get (use-package-normalize/:general
                        nil
                        nil
                        '((general-def 'normal "key" #'command)))
                       :commands)
            :to-equal '(command))
    (expect (plist-get (use-package-normalize/:general
                        nil
                        nil
                        '((general-def 'normal general-temp-map
                            "key" #'command)))
                       :commands)
            :to-equal '(command))
    (expect (plist-get (use-package-normalize/:general
                        nil
                        nil
                        '((general-defs
                            'normal
                            "key1" #'command1
                            'visual
                            "key2" #'command2)))
                       :commands)
            :to-equal '(command1 command2)))
  (it "should correctly extract symbols/commands to create autoloads from"
    (expect (general--extract-autoloadable-symbol nil)
            :to-be nil)
    (expect (general--extract-autoloadable-symbol "macro")
            :to-be nil)
    (expect (general--extract-autoloadable-symbol [?m ?a ?c ?r ?o])
            :to-be nil)
    (expect (general--extract-autoloadable-symbol general-temp-map)
            :to-be nil)
    (expect (general--extract-autoloadable-symbol (lambda () (interactive)))
            :to-be nil)
    (expect (general--extract-autoloadable-symbol '(menu-item))
            :to-be nil)
    (expect (general--extract-autoloadable-symbol 'symbol/command)
            :to-equal 'symbol/command)
    ;; conses
    (expect (general--extract-autoloadable-symbol
             '("describe keybindings" . general-describe-keybindings))
            :to-equal 'general-describe-keybindings)
    ;; not sure of the exact syntax since this type of binding is broken in
    ;; recent emacs versions
    (expect (general--extract-autoloadable-symbol
             '(fake-map . "char"))
            :to-equal nil)
    (expect (general--extract-autoloadable-symbol
             '(:ignore t :wk "replacement"))
            :to-be nil)
    (expect (general--extract-autoloadable-symbol
             '(:keymap create-autoload-map))
            :to-be nil)
    ;; created by general; don't need autoloads
    (expect (general--extract-autoloadable-symbol
             '(:prefix-command create-prefix-command))
            :to-be nil)
    (expect (general--extract-autoloadable-symbol
             '(nil :keyword val))
            :to-be nil)
    (expect (general--extract-autoloadable-symbol
             '(:def nil :keyword val))
            :to-be nil)
    (expect (general--extract-autoloadable-symbol
             '(:def "macro" :wk "replacement"))
            :to-be nil)
    (expect (general--extract-autoloadable-symbol
             '(:def [?m ?a ?c ?r ?o] :wk "replacement"))
            :to-be nil)
    (expect (general--extract-autoloadable-symbol
             (list :def general-temp-map :wk "replacement"))
            :to-be nil)
    (expect (general--extract-autoloadable-symbol
             '(:def (lambda () (interactive)) :wk "replacement"))
            :to-be nil)
    (expect (general--extract-autoloadable-symbol
             '(:def (menu-item) :wk "replacement"))
            :to-be nil)
    (expect (general--extract-autoloadable-symbol
             '(symbol/command :wk "replacement"))
            :to-equal 'symbol/command)
    (expect (general--extract-autoloadable-symbol
             '(:def symbol/command :wk "replacement"))
            :to-equal 'symbol/command)
    ;; conses
    (expect (general--extract-autoloadable-symbol
             '(("describe keybindings" . general-describe-keybindings)
               :keyword val))
            :to-equal 'general-describe-keybindings)
    (expect (general--extract-autoloadable-symbol
             '(:def ("describe keybindings" . general-describe-keybindings)))
            :to-equal 'general-describe-keybindings)
    (expect (general--extract-autoloadable-symbol
             '(:def (fake-map . "char")))
            :to-equal nil)))

;; *** :ghook
(describe "the :ghook use-package keyword"
  (before-all
    (defvar general-hook1 nil)
    (defvar general-hook2 nil)
    (defvar general-hook3 nil)
    (defvar general-hook4 nil)
    (defvar general-hooks '(general-hook1 general-hook2)))
  (before-each
    (setq general-hook1 nil
          general-hook2 nil
          general-hook3 nil
          general-hook4 nil))
  ;; :defer t isn't necessary since it is implied for :ghook
  (describe "specified with a variable"
    (it "should infer the function to add based on the package name"
      (use-package general-fake
        :ghook general-hooks)
      (expect general-hook1
              :to-equal '(general-fake-mode))
      (expect general-hook2
              :to-equal '(general-fake-mode))
      ;; test with `let'
      (let ((hooks '(general-hook3 general-hook4)))
        (use-package general-fake-mode
          :ghook hooks))
      ;; should not add an extra -mode
      (expect general-hook3
              :to-equal '(general-fake-mode))
      (expect general-hook4
              :to-equal '(general-fake-mode))))
  (describe "specified with a hook symbol"
    (it "should infer the function to add based on the package name"
      (use-package general-fake
        :ghook 'general-hook1)
      (expect general-hook1
              :to-equal '(general-fake-mode))
      (use-package general-fake-mode
        :ghook 'general-hook2)
      ;; should not add an extra -mode
      (expect general-hook2
              :to-equal '(general-fake-mode))))
  (describe "specified with a `general-add-hook' arglist"
    (it "should infer the function when there is no function arg"
      (use-package general-fake
        :ghook
        ('general-hook1))
      (expect general-hook1
              :to-equal '(general-fake-mode)))
    (it "should infer the function when the function is null or a non-symbol"
      (use-package general-fake
        :ghook
        ('general-hook1 nil)
        ('general-hook2 ""))
      (expect general-hook1
              :to-equal '(general-fake-mode))
      (expect general-hook2
              :to-equal '(general-fake-mode)))
    (it "should work with a list of hooks"
      (use-package general-fake
        :ghook
        ('(general-hook1 general-hook2)))
      (expect general-hook1
              :to-equal '(general-fake-mode))
      (expect general-hook2
              :to-equal '(general-fake-mode)))
    (it "should work, for example, with variable containing a list of hooks"
      (use-package general-fake
        :ghook
        (general-hooks))
      (expect general-hook1
              :to-equal '(general-fake-mode))
      (expect general-hook2
              :to-equal '(general-fake-mode)))
    (it "should work with explicitly specified functions"
      (use-package general-fake
        :ghook
        ('general-hook1 #'general-fake-func1)
        ('general-hook1 '(general-fake-func2 general-fake-func3))
        ('general-hook1 (list #'general-fake-func4))
        ('general-hook1 (lambda ())))
      (expect general-hook1
              :to-equal `(,(lambda ())
                          general-fake-func4
                          general-fake-func3
                          general-fake-func2
                          general-fake-func1)))
    (it "should support the extra APPEND (and LOCAL) args"
      (use-package general-fake
        :ghook
        ('general-hook1 #'general-fake-func1 t)
        ('general-hook1 #'general-fake-func2 t))
      (expect general-hook1
              :to-equal '(general-fake-func1
                          general-fake-func2)))
    (it "should add autoloads for non-lambda functions"
      (expect (not (functionp #'general-autoload-me-mode)))
      (expect (not (functionp #'general-autoload-me1)))
      (expect (not (functionp #'general-autoload-me2)))
      (expect (not (functionp #'general-autoload-me3)))
      (expect (not (functionp #'general-autoload-me4)))
      (defvar general-some-var nil)
      (defun general-some-call ())
      (use-package general-autoload-me
        :ghook 'general-hook1
        ('general-hook1 general-some-var)
        ('general-hook1 (lambda ()))
        ('general-hook1 #'general-autoload-me1)
        ('general-hook1 'general-autoload-me2)
        ('general-hook1 '(general-autoload-me3))
        ('general-hook1 (list #'general-autoload-me4 general-some-var
                              (general-some-call) (lambda ()))))
      (expect (not (functionp 'general-some-var)))
      (expect (functionp #'general-autoload-me-mode))
      (expect (functionp #'general-autoload-me1))
      (expect (functionp #'general-autoload-me2))
      (expect (functionp #'general-autoload-me3))
      (expect (functionp #'general-autoload-me4)))
    (it "should ignore macro/function calls and not fail"
      (use-package general-autoload-me
        :ghook ('general-hook1 (progn
                                 (defun some-func ())
                                 #'some-func))))))

;; *** :gfhook
(describe "the :gfhook use-package keyword"
  (before-all
    (defvar general-fake-mode-hook nil)
    (defvar general-hook1 nil)
    (defvar general-fake-functions '(general-fake-func1 general-fake-func2)))
  (before-each
    (setq general-fake-mode-hook nil
          general-hook1 nil))
  (describe "specified with a variable"
    (it "should infer the hook to add to based on the package name"
      (use-package general-fake
        :defer t
        :gfhook general-fake-functions)
      (expect general-fake-mode-hook
              :to-equal '(general-fake-func2 general-fake-func1))))
  (describe "specified with a function symbol"
    (it "should infer the hook to add to based on the package name"
      (use-package general-fake
        :defer t
        :gfhook #'general-fake-func1)
      (expect general-fake-mode-hook
              :to-equal '(general-fake-func1))
      (use-package general-fake-mode
        :defer t
        :gfhook
        'general-fake-func2
        #'general-fake-func3)
      ;; should not add an extra -mode
      (expect general-fake-mode-hook
              :to-equal '(general-fake-func3
                          general-fake-func2
                          general-fake-func1))))
  (describe "specified with a `general-add-hook' arglist"
    (it "should infer the hook when its arg is null or a non-symbol"
      (use-package general-fake
        :defer t
        :gfhook
        (nil 'general-fake-func1)
        ("" 'general-fake-func2))
      (expect general-fake-mode-hook
              :to-equal '(general-fake-func2
                          general-fake-func1)))
    (it "should work with a list of functions"
      (use-package general-fake
        :defer t
        :gfhook
        (nil '(general-fake-func1 general-fake-func2))
        (nil (list #'general-fake-func3 #'general-fake-func4))
        (nil (lambda ())))
      (expect general-fake-mode-hook
              :to-equal `(,(lambda ())
                          general-fake-func4
                          general-fake-func3
                          general-fake-func2
                          general-fake-func1)))
    (it "should work, for example, with a macro that expands to a function"
      (defmacro disable (mode)
        `(lambda () (,mode -1)))
      (use-package general-fake
        :defer t
        :gfhook
        (nil (disable visual-line-mode)))
      (expect general-fake-mode-hook
              :to-equal `(,(lambda () (visual-line-mode -1)))))
    (it "should work with explicitly specified hooks"
      (use-package general-fake
        :defer t
        :gfhook
        ('general-hook1 #'general-fake-func1)
        ('general-hook1 '(general-fake-func2 general-fake-func3))
        ('general-hook1 (list #'general-fake-func4)))
      (expect general-hook1
              :to-equal '(general-fake-func4
                          general-fake-func3
                          general-fake-func2
                          general-fake-func1)))
    (it "should support the extra APPEND (and LOCAL) args"
      (use-package general-fake
        :defer t
        :gfhook
        (nil #'general-fake-func1 t)
        (nil #'general-fake-func2 t))
      (expect general-fake-mode-hook
              :to-equal '(general-fake-func1
                          general-fake-func2)))
    (it "should NOT add autoloads for any functions"
      (use-package general-fake
        :defer t
        :gfhook 'general-undefined)
      (expect (not (functionp #'general-undefined))))
    (it "should not imply :defer t"
      ;; package doesn't exist, so should warn
      (spy-on 'display-warning)
      (use-package general-fake
        :gfhook 'general-fake-func1)
      (expect 'display-warning :to-have-been-called))))

;; * Global Override Mode
(describe "keybindings in `general-override-mode-map'"
  (it "should override keybindings defined in other minor mode keymaps"
    (general-define-key
     :keymaps 'general-test-mode-map
     "C-a" #'forward-char
     "C-b" #'forward-char)
    (general-define-key
     :keymaps 'general-override-mode-map
     "C-a" #'backward-char)
    ;; test keymap alias
    (general-define-key
     :keymaps 'override
     "C-b" #'backward-char)
    (general-override-mode)
    (expect (general-with "foo|"
              (evil-local-mode -1)
              "C-a C-b")
            :to-equal "f|oo")
    (general-override-mode -1)))

;; * Displaying keybindings
;; TODO

;; * Other Key Definition Helpers
;; ** Key Simulation
;; TODO test with 'self-insert-command
;; TODO test when simulate command bound to a multi-key sequence
;; TODO test repeat with different LOOKUP args
;; TODO test case where STATE and no KEYMAPS (e.g. normal should inherit from
;; motion)
(describe "general-simulate-key"
  (after-each
    (setq general-temp-map (make-sparse-keymap)))
  (it "should automatically generate named functions"
    (general-define-key
     :states 'normal
     :keymaps 'general-temp-map
     "a" (general-simulate-key "a")
     "b" (general-simulate-key "b" :state 'emacs)
     "c" (general-simulate-key "c" :keymap some-map)
     "d" (general-simulate-key "d" :state 'insert :keymap some-map)
     "e" (general-simulate-key (#'evil-delete "iw")))
    (expect (general-test-keys 'normal general-temp-map
              "a" #'general-simulate-a
              "b" #'general-simulate-b-in-emacs-state
              "c" #'general-simulate-c-in-some-map
              "d" #'general-simulate-d-in-insert-state-in-some-map
              "e" #'general-simulate-evil-delete-iw)))
  (it "should allow explicitly specifying a function name"
    (general-define-key
     :states 'normal
     :keymaps 'general-temp-map
     "a" (general-simulate-key "C-c" :name general-C-c))
    (expect (general-test-keys 'normal general-temp-map
              "a" #'general-C-c)))
  (it "should allow specifying a which-key description"
    (setq which-key-replacement-alist nil)
    (general-simulate-key "a" :which-key "simulate a")
    (expect which-key-replacement-alist
            :to-equal '(((nil . "general-simulate-a")
                         nil . "simulate a"))))
  (describe "should simulate the keys for a complete binding"
    (it "in the specified keymap"
      (general-define-key
       :states 'normal
       :keymaps 'general-test-mode-map
       "a" (general-simulate-key "j" :keymap evil-motion-state-map))
      (expect (general-with "|one\ntwo"
                "a")
              :to-equal "one\n|two"))
    (it "in the specified state"
      (general-define-key
       :states 'normal
       :keymaps 'general-test-mode-map
       "a" (general-simulate-key "C-n" :state 'emacs))
      (expect (general-with "|one\ntwo" "a")
              :to-equal "one\n|two")
      (general-define-key
       :states 'normal
       :keymaps 'general-test-mode-map
       ;; checks inheritance from motion state
       "a" (general-simulate-key "j" :state 'normal))
      (expect (general-with "|one\ntwo" "a")
              :to-equal "one\n|two"))
    (it "in the specified state and keymap"
      (general-define-key
       :states 'normal
       :keymaps 'general-temp-map
       "w" #'evil-forward-word-begin)
      (general-define-key
       :states 'normal
       :keymaps 'general-test-mode-map
       "a" (general-simulate-key "w" :state 'normal :keymap general-temp-map))
      (expect (general-with "|one two" "a")
              :to-equal "one |two")))
  (it "should allow state and keymap aliases"
    (general-define-key
     :states 'normal
     :keymaps 'general-temp-map
     "w" #'evil-forward-word-begin)
    (general-define-key
     :states 'normal
     :keymaps 'general-test-mode-map
     "a" (general-simulate-key "w" :state 'n :keymap temp))
    (expect (general-with "|one two" "a")
            :to-equal "one |two"))
  ;; TODO haven't found a way to test incomplete bindings
  ;; if is possible, add tests for repeating (e.g. "di" then "w")
  (describe "should simulate the keys for an incomplete binding"
    (xit "in the specified state and keymap"
      (general-define-key
       :states 'normal
       :keymaps 'general-temp-map
       "bc" #'next-line)
      (general-define-key
       :states 'normal
       :keymaps 'general-test-mode-map
       "a" (general-simulate-key "b" :state 'normal :keymap general-temp-map))
      (expect (general-with "|one\ntwo"
                ;; "ac"
                ;; (general--simulate-keys nil "ac")
                (general--simulate-keys nil "a")
                (general--simulate-keys nil "c"))
              :to-equal "one\n|two")))
  ;; NOTE this can't be tested either even though it works below with a count
  (xit "should work when a command is specified"
    (general-define-key
     :states 'normal
     :keymaps 'general-test-mode-map
     "a" (general-simulate-key (#'evil-delete "iw")))
    (expect (general-with "|one two" "a")
            :to-equal "| two"))
  (it "should work with a prefix argument"
    (general-define-key
     :states 'normal
     :keymaps 'general-test-mode-map
     "a" (general-simulate-key "j" :keymap evil-motion-state-map)
     "b" (general-simulate-key (#'evil-delete "iw")))
    (expect (general-with "|one\ntwo\nthree"
              "2a")
            :to-equal "one\ntwo\n|three")
    (expect (general-with "|one two"
              "2b")
            :to-equal "|two")
    ;; should not affect next command
    (let ((general--simulate-as-is t))
      (expect (general-with "|one\ntwo\nthree\nfour\nfive"
                "2aa")
              :to-equal "one\ntwo\nthree\n|four\nfive")
      (expect (general-with "|one two\nthree\nfour"
                "2ba")
              :to-equal "two\n|three\nfour")))
  ;; TODO a way to actually test recording the macro
  (describe "run during macro and evil-repeat recording/playback"
    (it "should work in the basic case"
      (general-define-key
       :states 'normal
       :keymaps 'general-test-mode-map
       "a" (general-simulate-key "j" :keymap evil-motion-state-map))
      (expect (general-with "|one\ntwo\nthree"
                (evil-declare-repeat #'evil-next-line)
                "a.")
              :to-equal "one\ntwo\n|three")
      (evil-declare-motion #'evil-next-line))
    ;; can't test but works manually
    (xit "should work when a command is specified"
      (general-define-key
       :states 'normal
       :keymaps 'general-test-mode-map
       "a" (general-simulate-key (#'evil-delete "iw")))
      (expect (general-with "|one two" "a.")
              :to-equal "|two"))
    (it "should work with a prefix argument"
      (general-define-key
       :states 'normal
       :keymaps 'general-test-mode-map
       "a" (general-simulate-key "j" :keymap evil-motion-state-map))
      (evil-declare-repeat #'evil-next-line)
      (expect (general-with "|one\ntwo\nthree\nfour\nfive" "2a.")
              :to-equal "one\ntwo\nthree\nfour\n|five")
      (evil-declare-motion #'evil-next-line)
      ;; (expect (general-with "|one\ntwo\nthree\nfour\nfive"
      ;;           (evil-set-register ?\q "2a")
      ;;           "2a@q")
      ;;         :to-equal "one\ntwo\nthree\nfour\n|five")
      )
    (it "should test whether the command is repeatable when lookup is used"
      (general-define-key
       :states 'normal
       :keymaps 'general-test-mode-map
       "a" (general-simulate-key "j" :keymap evil-motion-state-map)
       "b" (general-simulate-key "k" :keymap evil-motion-state-map))
      (evil-declare-repeat #'evil-next-line)
      (let ((general--simulate-as-is t))
        ;; should repeat next line and not previous line
        (expect (general-with "|one\ntwo\nthree\nfour\nfive" "2ab.")
                :to-equal "one\ntwo\nthree\n|four\nfive"))
      (evil-declare-motion #'evil-next-line)))
  (it "should optionally check to see if commands have been remapped"
    (general-define-key
     :states 'normal
     :keymaps 'general-test-mode-map
     "a" (general-simulate-key ('left-char "C-q a"))
     [remap left-char] #'right-char)
    (expect (general-with "b|b" "a")
            :to-equal "bba|")
    (general-define-key
     :states 'normal
     :keymaps 'general-test-mode-map
     "a" (general-simulate-key ('left-char "C-q a") :remap nil))
    (expect (general-with "b|b" "a")
            :to-equal "a|bb")
    (general-define-key
     :states 'normal
     :keymaps 'general-test-mode-map
     [remap left-char] nil)))

(describe "general-key"
  (it "should support custom setup and teardown"
    (general-define-key
     :states 'normal
     :keymaps 'general-test-mode-map
     "C-a" (general-key "C-f")
     "C-f" #'backward-char)
    (expect (general-with "fo|o"
              "C-a")
            :to-equal "f|oo")

    (general-define-key
     :states 'normal
     :keymaps 'general-test-mode-map
     "C-a" (general-key "C-f"
             :setup (evil-local-mode -1)
             :teardown (evil-local-mode)))
    (expect (general-with "fo|o"
              "C-a")
            :to-equal "foo|")
    (expect (general-with "fo|o"
              "C-a" "C-f")
            :to-equal "fo|o")))

;; ** General Key Dispatch
;; TODO
;; - add tests for REMAP

;; ** General Predicate Dispatch
(describe "general-predicate-dispatch"
  (it "should allow dispatching to multiple definitions based on predicates"
    (general-define-key
     :states 'normal
     :keymaps 'general-test-mode-map
     "<right>"
     (general-predicate-dispatch #'right-char
       :docstring "Move right or to the bol."
       (eolp) #'beginning-of-line))
    (expect (general-with "|foo" "<right>")
            :to-equal "f|oo")
    (expect (general-with "foo|"
              "<right>")
            :to-equal "|foo")))

;; ** Translate Key
(describe "general-translate-key"
  (before-each
    (general-define-key
     :keymaps '(general-temp-map general-test-mode-map)
     "a" #'a
     "b" #'b
     "c" #'c)
    (general-define-key
     :states '(normal visual)
     :keymaps 'general-temp-map
     "a" #'a
     "b" #'b
     "c" #'c))
  (after-each
    (setq general-temp-map (make-sparse-keymap)))
  (it "should bind each key to the definition of another key in the same keymap"
    (general-translate-key nil '(general-temp-map general-test-mode-map)
      "a" "b"
      "b" "c"
      "c" "a")
    (expect (general-test-keys nil (list general-temp-map
                                         general-test-mode-map)
              "a" #'b
              "b" #'c
              "c" #'a))
    (general-translate-key '(normal visual) 'general-temp-map
      "a" "b"
      "b" "c"
      "c" "a")
    (expect (general-test-keys '(normal visual) general-temp-map
              "a" #'b
              "b" #'c
              "c" #'a)))
  (it "should support translating keys using the original keymap for reference"
    (general-translate-key nil 'general-temp-map
      "a" "b")
    (general-translate-key nil 'general-temp-map
      "b" "c")
    (general-translate-key nil 'general-temp-map
      "c" "a")
    (expect (general-test-keys nil general-temp-map
              "a" #'b
              "b" #'c
              "c" #'a)))
  (it "should support destructively translating keys"
    (general-translate-key nil 'general-temp-map
      :destructive t
      "a" "b")
    (general-translate-key nil 'general-temp-map
      :destructive t
      "b" "c")
    (general-translate-key nil 'general-temp-map
      :destructive t
      "c" "a")
    (expect (general-test-keys nil general-temp-map
              "a" #'b
              "b" #'c
              "c" #'b)))
  (it "should support keymap and state aliases"
    (general-translate-key 'n 'temp
      "a" "b"
      "b" "c"
      "c" "a")
    (expect (general-test-keys 'normal general-temp-map
              "a" #'b
              "b" #'c
              "c" #'a)))
  (xit "should support 'local and 'global")
  (it "should use kbd when `general-implicit-kbd' is non-nil"
    (general-translate-key nil 'general-temp-map
      "C-a" "a")
    (expect (general-test-keys nil general-temp-map
              "C-a" #'a))
    (let (general-implicit-kbd)
      (general-translate-key nil 'general-temp-map
        (kbd "C-b") "b")
      (expect (general-test-keys nil general-temp-map
                "C-b" #'b))))
  (it "should just make the backup keymap if MAPS and DESCTRUCTIVE are nil"
    (makunbound 'general-general-temp-map-backup-map)
    (general-translate-key nil 'general-temp-map)
    (expect (boundp 'general-general-temp-map-backup-map)))
  (it "should allow unbinding keys"
    (general-define-key :keymaps 'general-temp-map
      "a" #'a)
    (general-test-keys nil general-temp-map
      "a" #'a)
    (general-translate-key nil 'general-temp-map
      "a" nil)
    (general-test-keys nil general-temp-map
      "a" nil)))

(describe "general-swap-key"
  (after-each
    (setq general-temp-map (make-sparse-keymap)))
  (it "should swap keys in a keymap"
    (general-define-key
     :keymaps 'general-temp-map
     "a" #'a
     "b" #'b)
    (general-swap-key nil 'general-temp-map
      :destructive t
      "a" "b")
    (expect (general-test-keys nil general-temp-map
              "a" #'b
              "b" #'a))
    (general-define-key
     :states 'normal
     :keymaps 'general-temp-map
     "a" #'a
     "b" #'b)
    (general-swap-key 'normal 'general-temp-map
      :destructive t
      "a" "b")
    (expect (general-test-keys 'normal general-temp-map
              "a" #'b
              "b" #'a))))

;; ** Automatic Unbinding
(describe "general-auto-unbind-keys"
  (after-each
    (setq general-temp-map (make-sparse-keymap)))
  (it "should automatically unbind non-prefix keys to prevent errors"
    (define-key general-temp-map "a" #'a)
    (expect (general-define-key
             :keymaps 'general-temp-map
             "ab" #'ab)
            :to-throw)
    (general-auto-unbind-keys)
    (expect (define-key general-temp-map "ab" #'ab)
            :to-throw)
    ;; general--definer-p
    (expect (general-define-key
             :keymaps 'general-temp-map
             "ab" #'ab
             "abc" #'abc
             ;; vector keys
             [97 98 99 100] #'abcd)
            :not :to-throw)
    (general-auto-unbind-keys t)
    (expect (general-define-eky
             :keymaps 'general-temp-map
             "abcde" #'abcde)
            :to-throw)))

;; ** Key-chord Helper
;; TODO

;; * Other Configuration Helpers
;; ** Settings
(describe "general-setq"
  (it "should act as a drop in replacement for setq"
    (defvar general-dummy-var-a nil)
    (defvar general-dummy-var-b nil)
    (general-setq general-dummy-var-a t
                  general-dummy-var-b t)
    (expect general-dummy-var-a)
    (expect general-dummy-var-b)
    (makunbound 'general-dummy-var-a)
    (makunbound 'general-dummy-var-b))
  (it "should correctly use a defined variable's custom setter"
    (defcustom general-dummy-var-with-setter nil
      ""
      :group 'general
      :set (lambda (sym _val)
             (set-default 'general-dummy-var-with-setter
                          1)))
    (general-setq general-dummy-var-with-setter 'not-1)
    (expect general-dummy-var-with-setter
            :to-equal 1)
    (makunbound 'general-dummy-var-with-setter))
  (it "should work for an undefined variable with a custom setter"
    (expect (not (boundp 'general-dummy-var-with-setter)))
    (general-setq general-dummy-var-with-setter 'not-1)
    (defcustom general-dummy-var-with-setter nil
      ""
      :group 'general
      :set (lambda (sym _val)
             (set-default 'general-dummy-var-with-setter
                          1)))
    (expect general-dummy-var-with-setter
            :to-equal 1)
    (makunbound 'general-dummy-var-with-setter)))

;; ** Hooks
(describe "general-add-hook and general-remove-hook"
  (before-each
    (makunbound 'general--test-hook)
    (defvar general--test-hook nil)
    (makunbound 'general--test-hook2)
    (defvar general--test-hook2 nil))
  (it "should act as a drop in replacement for `add-hook' or `remove-hook'"
    ;; add
    (general-add-hook 'general--test-hook #'b)
    (general-add-hook 'general--test-hook #'a)
    (general-add-hook 'general--test-hook #'c t)
    (expect general--test-hook
            :to-equal (list #'a #'b #'c))
    (with-temp-buffer
      (general-add-hook 'general--test-hook #'d t))
    (expect general--test-hook
            :to-equal (list #'a #'b #'c #'d))
    (with-temp-buffer
      (general-add-hook 'general--test-hook #'e t t))
    (expect general--test-hook
            :to-equal (list #'a #'b #'c #'d))
    ;; remove
    (general-remove-hook 'general--test-hook #'a)
    (general-remove-hook 'general--test-hook #'b)
    (general-remove-hook 'general--test-hook #'c)
    (general-remove-hook 'general--test-hook #'d)
    (expect (null general--test-hook)))
  (it "should allow the hooks and functions to be lists"
    ;; add
    (general-add-hook '(general--test-hook
                        general--test-hook2)
                      (list #'b #'a))
    (expect general--test-hook
            :to-equal (list #'a #'b))
    (expect general--test-hook2
            :to-equal (list #'a #'b))
    (general-add-hook '(general--test-hook
                        general--test-hook2)
                      (list #'c #'d)
                      t)
    (expect general--test-hook
            :to-equal (list #'a #'b #'c #'d))
    (expect general--test-hook2
            :to-equal (list #'a #'b #'c #'d))
    ;; remove
    (general-remove-hook '(general--test-hook
                           general--test-hook2)
                         (list #'a #'b #'c #'d))
    (expect (null general--test-hook))
    (expect (null general--test-hook2)))
  (it "should allow lambdas as functions"
    ;; add
    (general-add-hook 'general--test-hook (lambda ()))
    (expect general--test-hook
            :to-equal (list (lambda ())))
    (general-add-hook 'general--test-hook (list (lambda () 1)) t)
    (expect general--test-hook
            :to-equal (list (lambda ())
                            (lambda () 1)))
    ;; remove
    (general-remove-hook 'general--test-hook (lambda ()))
    (general-remove-hook 'general--test-hook (list (lambda () 1)))
    (expect (null general--test-hook)))
  (describe "should allow creating \"transient\" hooks"
    (it "that are removed after one run"
      (let ((test-val 0))
        (general-add-hook 'general--test-hook (lambda () (cl-incf test-val))
                          nil nil t)
        (run-hooks 'general--test-hook)
        (run-hooks 'general--test-hook)
        (expect test-val
                :to-equal 1)))
    (it "that are removed after success"
      (let ((test-val 0))
        (general-add-hook 'general--test-hook
                          (lambda ()
                            (cl-incf test-val)
                            (if (= test-val 2)
                                t
                              nil))
                          nil nil #'identity)
        (run-hooks 'general--test-hook)
        (run-hooks 'general--test-hook)
        (run-hooks 'general--test-hook)
        (expect test-val
                :to-equal 2)))
    (it "that are removed after any other condition"
      (let ((test-val 0))
        (general-add-hook 'general--test-hook
                          (lambda ()
                            (cl-incf test-val))
                          nil nil (lambda (val) (= val 3)))
        (run-hooks 'general--test-hook)
        (run-hooks 'general--test-hook)
        (run-hooks 'general--test-hook)
        (run-hooks 'general--test-hook)
        (expect test-val
                :to-equal 3)))))

;; ** Advice
(describe "general-advice-add and general-advice-remove"
  (before-all
    (define-error 'general--test-error "Error")
    (define-error 'general--test-error2 "Error 2")
    (defun general--error ()
      (signal 'general--test-error nil))
    (defun general--error2 ()
      (signal 'general--test-error2 nil)))
  (before-each
    (fmakunbound 'general--test-func)
    (setplist 'general--test-func nil)
    (defun general--test-func () 1)
    (fmakunbound 'general--test-func2)
    (setplist 'general--test-func nil)
    (defun general--test-func2 () 2))
  (it "should act as a drop in replacement for `advice-add' or `advice-remove'"
    ;; add
    (general-advice-add 'general--test-func :before #'general--error)
    (expect (general--test-func)
            :to-throw 'general--test-error)
    ;; remove
    (general-advice-remove 'general--test-func #'general--error)
    (expect (general--test-func)
            :to-equal 1))
  (it "should allow the symbols and functions to be lists"
    ;; add
    (general-advice-add '(general--test-func
                          general--test-func2)
                        :before
                        (list #'general--error #'general--error2))
    (expect (general--test-func)
            :to-throw 'general--test-error2)
    (expect (general--test-func2)
            :to-throw 'general--test-error2)
    ;; remove
    (general-advice-remove '(general--test-func general--test-func2)
                           #'general--error2)
    (expect (general--test-func)
            :to-throw 'general--test-error)
    (expect (general--test-func2)
            :to-throw 'general--test-error)
    (general-advice-add '(general--test-func
                          general--test-func2)
                        :before
                        (list #'general--error #'general--error2))
    (general-advice-remove '(general--test-func general--test-func2)
                           (list #'general--error #'general--error2))
    (expect (general--test-func)
            :to-equal 1)
    (expect (general--test-func2)
            :to-equal 2))
  (it "should allow lambdas as functions"
    ;; add
    (general-advice-add 'general--test-func
                        :before (lambda () (signal 'general--test-error nil)))
    (expect (general--test-func)
            :to-throw 'general--test-error)
    (general-advice-add 'general--test-func
                        :before
                        (list (lambda () (signal 'general--test-error2 nil))))
    (expect (general--test-func)
            :to-throw 'general--test-error2)
    ;; remove
    (general-remove-advice 'general--test-func
                           (lambda () (signal 'general--test-error nil)))
    (general-remove-advice 'general--test-func
                           (list (lambda () (signal 'general--test-error2 nil))))
    (expect (general--test-func)
            :to-equal 1))
  (describe "should allow creating \"transient\" advice"
    (it "that is removed after one run"
      (defun general-1 ()
        1)
      (general-add-advice 'general-1 :override (lambda (&rest _) 2)
                          nil t)
      (expect (general-1) :to-equal 2)
      (expect (general-1) :to-equal 1)
      (fmakunbound 'general-1))
    (it "that is removed after success"
      (let ((test-val 0))
        (defun general-1 ()
          1)
        (general-add-advice 'general-1
                            :override (lambda (&rest _)
                                        (if (= test-val 3)
                                            t
                                          (cl-incf test-val)
                                          nil))
                            nil #'identity)
        (expect (general-1) :to-equal nil)
        (expect (general-1) :to-equal nil)
        (expect (general-1) :to-equal nil)
        (expect (general-1) :to-equal t)
        (expect (general-1) :to-equal 1)
        (fmakunbound 'general-1)))
    (it "that is removed after any other condition"
      (let ((test-val 0))
        (defun general-1 ()
          1)
        (general-add-advice 'general-1
                            :override (lambda (&rest _)
                                        (cl-incf test-val))
                            nil (lambda (val) (= val 3)))
        (expect (general-1) :to-equal 1)
        (expect (general-1) :to-equal 2)
        (expect (general-1) :to-equal 3)
        (expect (general-1) :to-equal 1)
        (fmakunbound 'general-1)))))

(provide 'test-general)
;;; test-general.el ends here
