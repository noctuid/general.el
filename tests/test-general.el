;;; * Setup
(require 'general)
(require 'evil)

(setq evil-want-change-word-to-end nil
      evil-move-cursor-back nil)

(define-minor-mode general-test-mode
  "A minor mode for general tests."
  :lighter ""
  :keymap (make-sparse-keymap)
  (evil-normalize-keymaps))

(defvar general-temp-map (make-sparse-keymap))

;;; * Helpers
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
             (general-test-mode)
             (transient-mark-mode)
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

(defun general-test-evil-keys (state keymap &rest maps)
  "Look in the keymap for STATE and KEYMAP for MAPS."
  (declare (indent 2))
  (let (res)
    (while maps
      (push (eq (lookup-key (evil-get-auxiliary-keymap keymap state)
                            (kbd (pop maps)))
                (pop maps))
            res))
    (not (memq nil res))))

;;; * General Simulate Keys
(describe "general-simulate-keys"
  (before-all
    (evil-declare-repeat #'next-line)
    (evil-declare-repeat #'previous-line)
    (evil-declare-repeat #'evil-forward-word-begin)
    (general-emacs-define-key general-temp-map
      "n" #'previous-line)
    (general-evil-define-key 'insert general-temp-map
      "n" #'evil-forward-word-begin
      "ef" #'next-line)
    (general-evil-define-key 'normal general-test-mode-map
      "n" (general-simulate-keys "C-n")
      "N" (general-simulate-keys "C-n" t)
      "SPC" (general-simulate-keys "C-c" nil nil nil "" general-C-c)
      "h" (general-simulate-keys "n" nil general-temp-map)
      "k" (general-simulate-keys "n" 'insert general-temp-map)
      "l" (general-simulate-keys "e" 'insert general-temp-map)
      "m" (general-simulate-keys (#'evil-delete "iw"))
      "M" (general-simulate-keys "diw")))
  (after-all
    (evil-declare-motion #'next-line)
    (evil-declare-motion #'previous-line)
    (evil-declare-motion #'evil-forward-word-begin)
    (setq general-temp-map (make-sparse-keymap)
          general-test-mode-map (make-sparse-keymap)))
  (it "should automatically generate named functions"
    (expect (general-test-evil-keys 'normal general-test-mode-map
              "n" #'general-simulate-C-n
              "N" #'general-simulate-C-n-in-emacs-state
              "h" #'general-simulate-n-in-general-temp-map
              "k" #'general-simulate-n-in-insert-state-in-general-temp-map
              "m" #'general-simulate-evil-delete-iw)
            :to-be-truthy))
  (it "should allow explicitly specifying a function name"
    (expect (general-test-evil-keys 'normal general-test-mode-map
              "SPC" #'general-C-c)
            :to-be-truthy))
  (describe "should simulate the keys for a complete binding"
    (it "in the specified state"
      (expect (general-with "one\n|two" "h")
              :to-equal "|one\ntwo"))
    (it "in the specified keymap"
      (expect (general-with "|one\ntwo" "N")
              :to-equal "one\n|two"))
    (it "in the specified keymap and state"
      (expect (general-with "|one two" "k")
              :to-equal "one |two")))
  ;; haven't found a way to test automatically
  ;; if do find a way, test that "di" followed by "w" repeats correctly
  ;; (NO-LOOKUP must be used)
  ;; (describe "should simulate the keys for an incomplete binding"
  ;;   (it "in the specified state and keymap"
  ;;     (expect (general-with "|one\ntwo"
  ;;               (general--simulate-keys nil "l")
  ;;               (general--simulate-keys nil "f"))
  ;;             :to-equal "one\n|two")))
  (it "should work when a command is specified"
    (expect (general-with "|one two" "m")
            :to-equal "| two"))
  (it "should work with a prefix"
    (expect (general-with "|one\ntwo\nthree" "2N")
            :to-equal "one\ntwo\n|three")
    (expect (general-with "|one two" "2m")
            :to-equal "|two"))
  (describe "run during evil-repeat recording/playback"
    (it "should work in the specified state"
      (expect (general-with "one\ntwo\n|three" "h.")
              :to-equal "|one\ntwo\nthree")
      (expect (general-with "one\ntwo\nthree\nfour\n|five" "2h.")
              :to-equal "|one\ntwo\nthree\nfour\nfive"))
    (it "should work in the specified keymap"
      (expect (general-with "|one\ntwo\nthree" "N.")
              :to-equal "one\ntwo\n|three")
      (expect (general-with "|one\ntwo\nthree\nfour\nfive" "2N.")
              :to-equal "one\ntwo\nthree\nfour\n|five"))
    (it "should work in the specified state and keymap"
      (expect (general-with "|one two three" "k.")
              :to-equal "one two |three")
      (expect (general-with "|one two three four five" "2k.")
              :to-equal "one two three four |five"))
    (it "should work when a command is specified"
      (expect (general-with "|one two" "m.")
              :to-equal "|two")
      (expect (general-with "|one two three" "2m.")
              :to-equal "|three"))
    (it "should work when only keys are specified"
      (expect (general-with "|one two" "M.")
              :to-equal "|two")
      (expect (general-with "|one two three" "2M.")
              :to-equal "|three"))))
