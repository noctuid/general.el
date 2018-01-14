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
(require 'buttercup)
(require 'general)
(require 'evil)
(require 'which-key)
(require 'use-package)

(push "tests/" load-path)

(setq evil-want-change-word-to-end nil
      evil-move-cursor-back nil)

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
    (expect (general-with "fo|o"
              (evil-local-mode -1)
              ;; TODO this test works in open emacs but when running
              ;; (general-override-local-mode)
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
       "G" '(:keymap general-command-map :wk "general prefix"))
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

;; ** Vim Definers
(describe "wrappers created with `general-create-vim-definer'"
  (before-all
    (general-create-dual-vim-definer general-nmap 'normal)
    (general-create-vim-definer general-itomap 'evil-inner-text-objects-map))
  (it "should set a default :keymaps"
    (let ((evil-normal-state-map (make-sparse-keymap)))
      (general-nmap "a" #'a)
      (expect (general-test-keys nil evil-normal-state-map
                "a" #'a))
      (expect (general-test-keys 'normal (current-global-map)
                "a" nil))))
  (it "should set a default :states if `general-vim-definer-default' is 'states"
    (let ((evil-normal-state-map (make-sparse-keymap))
          (general-vim-definer-default 'states))
      (general-nmap "a" 'a)
      (expect (general-test-keys 'normal (current-global-map)
                "a" #'a))
      (expect (general-test-keys nil evil-normal-state-map
                "a" nil))
      (general-nmap "a" nil)))
  (it "should set a default :states if the user manually specifies keymaps"
    (let ((evil-normal-state-map (make-sparse-keymap)))
      (general-nmap general-test-mode-map "a" #'a)
      (general-nmap :keymaps 'general-test-mode-map "b" #'b)
      (expect (general-test-keys 'normal general-test-mode-map
                "a" #'a
                "b" #'b))
      (expect (general-test-keys nil evil-normal-state-map
                "a" nil
                "b" nil))))
  (it "should never set :states if none were given"
    (let ((evil-inner-text-objects-map (make-sparse-keymap))
          (evil-normal-state-map (make-sparse-keymap)))
      (let ((general-vim-definer-default 'states))
        (general-itomap "a" #'a))
      ;; should work even though not intended usage
      (general-itomap evil-normal-state-map "b" #'b)
      (general-itomap :keymaps 'evil-normal-state-map "c" #'c)
      (expect (general-test-keys nil evil-inner-text-objects-map
                "a" #'a
                "b" nil
                "c" nil))
      (expect (general-test-keys nil evil-normal-state-map
                "b" #'b
                "c" #'c)))))

;; ** Other User-created Definers
;; TODO

;; ** Use-package Keyword
(describe "the :general use-package keyword"
  (before-all
    (general-create-dual-vim-definer general-nmap 'normal))
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
  (it "should correctly extract symbols/commands to create autoloads from"
    (expect (general--extract-symbol nil)
            :to-be nil)
    (expect (general--extract-symbol "macro")
            :to-be nil)
    (expect (general--extract-symbol [?m ?a ?c ?r ?o])
            :to-be nil)
    (expect (general--extract-symbol general-temp-map)
            :to-be nil)
    (expect (general--extract-symbol (lambda () (interactive)))
            :to-be nil)
    (expect (general--extract-symbol '(menu-item))
            :to-be nil)
    ;; TODO conses
    (expect (general--extract-symbol 'symbol/command)
            :to-equal 'symbol/command)
    (expect (general--extract-symbol '(:ignore t :wk "replacement"))
            :to-equal nil)
    (expect (general--extract-symbol '(:keymap create-autoload-map))
            :to-equal nil)
    ;; created by general; don't need autoloads
    (expect (general--extract-symbol '(:prefix-command create-prefix-command))
            :to-equal nil)
    (expect (general--extract-symbol '(nil :keyword val))
            :to-equal nil)
    (expect (general--extract-symbol '(:def nil :keyword val))
            :to-equal nil)
    (expect (general--extract-symbol '(:def "macro" :wk "replacement"))
            :to-equal nil)
    (expect (general--extract-symbol '(:def [?m ?a ?c ?r ?o] :wk "replacement"))
            :to-equal nil)
    (expect (general--extract-symbol (list :def general-temp-map :wk "replacement"))
            :to-equal nil)
    (expect (general--extract-symbol '(:def (lambda () (interactive))
                                       :wk "replacement"))
            :to-equal nil)
    (expect (general--extract-symbol '(:def (menu-item) :wk "replacement"))
            :to-equal nil)
    ;; TODO conses
    (expect (general--extract-symbol '(:def symbol/command :wk "replacement"))
            :to-equal 'symbol/command)))

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
      (evil-declare-motion #'evil-next-line))))


;; ** General Key Dispatch
;; TODO

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
                "C-b" #'b)))))

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
;; TODO

;; ** Advice
;; TODO

(provide 'test-general)
;;; test-general.el ends here
