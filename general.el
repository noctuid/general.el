;;; general.el --- Convenience wrappers for keybindings.

;; Author: Fox Kiester <noct@openmailbox.org>
;; URL: https://github.com/noctuid/general.el
;; Created: February 17, 2016
;; Keywords: vim, evil, leader, keybindings, keys
;; Package-Requires: ((cl-lib "0.5"))
;; Version: 0.1

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
;; This package provides convenient wrappers for more succinctly defining
;; keybindings. It allows defining multiple keys at once, specifying an
;; arbitrary number of named prefix keys to be used in key definitions,
;; implicitly wrapping key strings with (kbd ...), and more. It provides a
;; single function for standard emacs key definitions as well as evil key
;; definitions for any evil state and any keymap. It also provides a setup
;; function to generate "nmap", "vmap", etc. keybinding functions for evil.

;; For more information see the README in the online repository.

;;; Code:
(require 'cl-lib)

(defgroup general nil
  "Gives convenient wrappers for key definitions."
  :group 'convenience
  :prefix 'general-)

(defcustom general-implicit-kbd t
  "Whether to implicitly wrap a (kbd) around keybindings.
This applies to the prefix key as well."
  :group 'general
  :type 'boolean)

(defcustom general-default-prefix nil
  "The default prefix key sequence to use."
  :group 'general
  :type 'string)

(defcustom general-default-non-normal-prefix nil
  "The default prefix key sequence to use for the 'emacs and 'insert states.
Note that this setting is only useful for evil-users and will only have an
effect when binding keys in the 'emacs and/or 'insert states or in the
'evil-insert-state-map and/or 'evil-emacs-state-map keymaps. When this is not
specified, `general-default-prefix' will be the default prefix for any states
and keymaps. If this is specified `general-default-prefix' or the arg to :prefix
will not be used when binding keys in the insert and emacs states."
  :group 'general
  :type 'string)

(defcustom general-default-global-prefix nil
  "The default prefix key sequence to use for all evil states.
This setting is only useful for evil users. Note that like with
`general-default-non-normal-prefix', if this or :global-prefix is specified,
`general-default-prefix' or the arg to :prefix will not be used for binding
keys in the insert and emacs states. If you don't need a different or extra
prefix for one or both state types (insert and emacs vs. the other states),
just use `general-default-prefix'/:prefix by itself."
  :group 'general
  :type 'string)

(define-widget 'general-state 'lazy
  "General's evil state type."
  :type '(choice
          (const :tag "Normal state" normal)
          (const :tag "Insert state" insert)
          (const :tag "Visual state" visual)
          (const :tag "Replace state" replace)
          (const :tag "Operator state" operator)
          (const :tag "Motion state" motion)
          (const :tag "Emacs state" emacs)
          (const :tag "Use define-key not evil-define-key" nil)))

(defcustom general-default-states nil
  "The default evil state(s) to make mappings in.
Non-evil users should keep this nil."
  :group 'general
  :type '(choice general-state
                 (set general-state)))

(define-widget 'general-keymap 'lazy
  "General's keymap type."
  :type '(choice
          (const :tag "Global keymap" global)
          (const :tag "Buffer local keymap" local)
          symbol))

(defcustom general-default-keymaps 'global
  "The default keymap(s) to bind keys in."
  :group 'general
  :type '(choice general-keymap
                 (repeat general-keymap)))

(defcustom general-vim-definer-default nil
  "Whether set the states or keymaps in a `general-create-vim-definer' function.
If nil, use the default from when the function was created. If 'keymaps, set the
default keymaps. If 'states, set the default states."
  :group 'general
  :type '(choice
          (const :tag "Default to setting :keymaps" keymaps)
          (const :tag "Default to setting :states" states)
          (const :tag "Use the initial default" nil)))

(defvar general-keybindings nil
  "Holds all the keybindings created with `general-define-key' (and wrappers).
This is an alist of a keymap to an alist of a state to keybindings.")

;;; Helpers
(defun general--apply-prefix-and-kbd (prefix maps)
  "Prepend the PREFIX sequence to all MAPS.
Adds a (kbd ...) if `general-implicit-kbd' is non-nil."
  (let ((prefix (or prefix "")))
    (mapcar (lambda (elem)
              (if (stringp elem)
                  (if general-implicit-kbd
                      (kbd (concat prefix " " elem))
                    (concat prefix elem))
                elem))
            maps)))

;; http://endlessparentheses.com/define-context-aware-keys-in-emacs.html
(defun general--apply-predicate (predicate maps)
  "Apply PREDICATE to the commands in MAPS."
  (mapcar (lambda (maybe-command)
            (if (not (stringp maybe-command))
                `(menu-item "" nil
                            :filter (lambda (&optional _)
                                      (when ,predicate
                                        (,maybe-command))))
              maybe-command))
          maps))

(defun general--record-keybindings (keymap state maps)
  "For KEYMAP and STATE, add MAPS to `general-keybindings'.
For non-evil keybindings, STATE will be nil."
  (unless (eq keymap 'local)
    (unless (assq keymap general-keybindings)
      (add-to-list 'general-keybindings (list keymap)))
    (unless (assq state (assq keymap general-keybindings))
      (setcdr (assq keymap general-keybindings)
              (list (list state))))
    (let ((state-cons (assq state (assq keymap general-keybindings))))
      (setcdr (assq state (assq keymap general-keybindings))
              (append (cdr state-cons) maps)))))

;; don't force non-evil user to require evil for one function (this is evil-delay)
(defun general--delay (condition form hook &optional append local name)
  "Execute FORM when CONDITION becomes true, checking with HOOK.
NAME specifies the name of the entry added to HOOK. If APPEND is
non-nil, the entry is appended to the hook. If LOCAL is non-nil,
the buffer-local value of HOOK is modified."
  (declare (indent 2))
  (if (and (not (booleanp condition)) (eval condition))
      (eval form)
    (let* ((name (or name (format "general-delay-form-in-%s" hook)))
           (fun (make-symbol name))
           (condition (or condition t)))
      (fset fun `(lambda (&rest args)
                   (when ,condition
                     (remove-hook ',hook #',fun ',local)
                     ,form)))
      (put fun 'permanent-local-hook t)
      (add-hook hook fun append local))))

;;; define-key and evil-define-key Wrappers
;; TODO better way to do this?
;; https://www.reddit.com/r/emacs/comments/1582uo/bufferlocalsetkey_set_a_key_in_one_buffer_only/
(defvar general--blm nil)

(defun general--generate-keymap-name (sym)
  "Generate a map name from SYM."
  (symbol-value (intern (concat (symbol-name sym) "-map"))))

(defun general--emacs-local-set-key (key func)
  "Bind KEY to FUNC for the current buffer only using a minor mode."
  (if general--blm
      (define-key (general--generate-keymap-name general--blm) key func)
    (let* ((mode-name-loc (cl-gensym "general-blm")))
      (eval `(define-minor-mode ,mode-name-loc nil nil nil (make-sparse-keymap)))
      (setq-local general--blm mode-name-loc)
      (funcall mode-name-loc 1)
      (define-key (general--generate-keymap-name general--blm) key func))))

;; this works but will make it so that keys defined for the major mode will no longer affect
;; (use-local-map (copy-keymap (current-local-map)))
;; (local-set-key (kbd "C-c y") 'helm-apropos)

(defun general--emacs-define-key (keymap &rest maps)
  "Wrapper for `define-key' and general's `general--emacs-local-set-key'.
KEYMAP determines which keymap the MAPS will be defined in. When KEYMAP is
is 'local, the MAPS will be bound only in the current buffer. MAPS is any
number of paired keys and commands"
  (declare (indent 1))
  (let (key func)
    (while (setq key (pop maps))
      (setq func (pop maps))
      (if (eq keymap 'local)
          (general--emacs-local-set-key key func)
        (define-key keymap key func)))))

;; can't apply evil-define-key since it is a macro
;; either need to use eval or splice (,@) with defmacro
;; or not make use of evil-define-key's &rest and repeatedly call it

;; (defmacro general-evil-define-key (prefix-key state keymap &rest maps)
;;   ;; needs special indent
;;   "A wrapper."
;;   (declare (indent 3))
;;   (setq prefix-key (or prefix-key ""))
;;   (setq maps (--map-when (stringp it ) (concat prefix-key it) maps))
;;   `(eval-after-load 'evil
;;      (evil-define-key ,state ,keymap ,@maps)))

(defun general--evil-define-key (state keymap &rest maps)
  "A wrapper for `evil-define-key' and `evil-local-set-key'.
STATE is the evil state to bind the keys in. `evil-local-set-key' is used when
KEYMAP is 'local. MAPS is any number of keys and commands to bind."
  (declare (indent defun))
  (eval-after-load 'evil
    `(let ((maps ',maps)
           (keymap ',keymap)
           (state ',state)
           key
           func)
       (while (setq key (pop maps))
         (setq func (pop maps))
         (if (eq keymap 'local)
             ;; has no &rest
             (evil-local-set-key state key func)
           (evil-define-key state keymap key func))))))

;;; Functions With Keyword Arguments
;;;###autoload
(cl-defun general-define-key
    (&rest maps &key (prefix general-default-prefix)
           (non-normal-prefix general-default-non-normal-prefix)
           (global-prefix general-default-global-prefix)
           (states general-default-states)
           (keymaps general-default-keymaps)
           (predicate)
           &allow-other-keys)
  "The primary key definition function provided by general.

PREFIX corresponds to a prefix key and defaults to none. STATES corresponds to
the evil state(s) to bind the keys in. Non-evil users should not set STATES.
When STATES is non-nil, `evil-define-key' will be used. Otherwise `define-key'
will be used. Evil users may also want to leave STATES nil and set KEYMAPS to
a keymap such as `evil-normal-state-map' for global mappings. KEYMAPS defaults
to `global-map'. Note that STATES and KEYMAPS can either be a list or a single
symbol. If any keymap does not exist, the keybindings will be deferred until
the keymap does exist, so using `eval-after-load' is not necessary with this
function.

If NON-NORMAL-PREFIX is specified, this prefix will be used for emacs and insert
state keybindings instead of PREFIX. This argument will only have an effect if
'insert and/or 'emacs is one of the STATES or if 'evil-insert-state-map and/or
'evil-emacs-state-map is one of the KEYMAPS. Alternatively, GLOBAL-PREFIX can be
used with PREFIX and/or NON-NORMAL-PREFIX to bind keys in all states under a
specified prefix. Like with NON-NORMAL-PREFIX, GLOBAL-PREFIX will prevent PREFIX
from applying to insert and emacs states. Note that these keywords are only
useful for evil users.

Unlike with normal key definitions functions, the keymaps in KEYMAPS should be
quoted (this makes it easy to check if there is only one keymap instead of a
list of keymaps).

MAPS will be recorded for later use with `general-describe-keybindings'."
  (let (non-normal-prefix-maps
        global-prefix-maps)
    ;; remove keyword arguments from rest var
    (setq maps
          (cl-loop for (key value) on maps by 'cddr
                   unless (keywordp key)
                   collect key
                   and collect value))
    ;; don't force the user to wrap a single state or keymap in a list
    ;; luckily nil is a list
    (unless (listp states)
      (setq states (list states)))
    (unless (listp keymaps)
      (setq keymaps (list keymaps)))
    (when predicate
      (setq maps (general--apply-predicate predicate maps)))
    (when non-normal-prefix
      (setq non-normal-prefix-maps
            (general--apply-prefix-and-kbd non-normal-prefix maps)))
    (when global-prefix
      (setq global-prefix-maps
            (general--apply-prefix-and-kbd global-prefix maps)))
    ;; last so not applying prefix twice
    (setq maps (general--apply-prefix-and-kbd prefix maps))
    (dolist (keymap keymaps)
      (if states
          (dolist (state states)
            (general--record-keybindings keymap state maps))
        (general--record-keybindings keymap nil maps))
      (general--delay `(or (eq ',keymap 'local)
                           (eq ',keymap 'global)
                           (and (boundp ',keymap)
                                (keymapp ,keymap)))
          `(let ((keymap (cond ((eq ',keymap 'local)
                                ;; keep keymap quoted if it was 'local
                                'local)
                               ((eq ',keymap 'global)
                                (current-global-map))
                               (t
                                ,keymap))))
             (if ',states
                 (dolist (state ',states)
                   (cond ((and (or ',non-normal-prefix-maps
                                   ',global-prefix-maps)
                               (member state '(insert emacs)))
                          (when ',non-normal-prefix-maps
                            (apply #'general--evil-define-key state keymap
                                   ',non-normal-prefix-maps)))
                         (t
                          (apply #'general--evil-define-key state keymap
                                 ',maps)))
                   (when ',global-prefix-maps
                     (apply #'general--evil-define-key state keymap
                            ',global-prefix-maps)))
               (cond ((and (or ',non-normal-prefix-maps
                               ',global-prefix-maps)
                           (member ',keymap '(evil-insert-state-map
                                              evil-emacs-state-map)))
                      (when ',non-normal-prefix-maps
                        (apply #'general--emacs-define-key keymap
                               ',non-normal-prefix-maps)))
                     (t
                      (apply #'general--emacs-define-key keymap ',maps)))
               (when ',global-prefix-maps
                 (apply #'general--emacs-define-key keymap
                        ',global-prefix-maps))))
        'after-load-functions t nil
        (format "general-define-key-in-%s" keymap)))))

;;;###autoload
(defmacro general-create-definer (name &rest args)
  "A helper macro to create key definitions functions.
This allows the creation of key definition functions that
will use a certain keymap, evil state, and/or prefix key by default.
NAME will be the function name and ARGS are the keyword arguments that
are intended to be the defaults."
  `(defun ,name (&rest args)
     ;; can still override keywords afterwards
     (apply #'general-define-key (append args (list ,@args)))))

;;;###autoload
(defmacro general-emacs-define-key (keymaps &rest args)
  "A wrapper for `general-define-key' that is similar to `define-key'.
It has a positional argument for KEYMAPS. It acts the same as
`general-define-key', and ARGS can contain keyword arguments in addition to
keybindings. This can basically act as a drop-in replacement for `define-key',
and unlike with `general-define-key', if KEYMAPS is a single keymap, it does
not need to be quoted."
  (declare (indent 1))
  `(general-define-key ,@args
                       :keymaps (if (symbolp ',keymaps)
                                    ',keymaps
                                  ,keymaps)))

;;;###autoload
(defmacro general-evil-define-key (states keymaps &rest args)
  "A wrapper for `general-define-key' that is similar to `evil-define-key'.
It has positional arguments for STATES and KEYMAPS. It acts the same as
`general-define-key', and ARGS can contain keyword arguments in addition to
keybindings. This can basically act as a drop-in replacement for
`evil-define-key', and unlike with `general-define-key', if KEYMAPS is a single
keymap, it does not need to be quoted."
  (declare (indent 2))
  `(general-define-key ,@args
                       :states ,states
                       :keymaps (if (symbolp ',keymaps)
                                    ',keymaps
                                  ,keymaps)))
;;; Displaying Keybindings
(defun general--print-keybind-table (maps)
  "Print an org table for MAPS."
  (princ "|key|command|\n|-+-|\n")
  (cl-loop for (key command) on maps by 'cddr
           do (princ (format "|=%s=|~%s~|\n" (key-description key) command)))
  (princ "\n"))

(defun general--print-state-heading (state-cons)
  "Print a table and possibly a heading for STATE-CONS."
  (let ((state (car state-cons))
        (maps (cdr state-cons)))
    (unless (null state)
      (princ (capitalize (concat "** " (symbol-name state) " State\n"))))
    (general--print-keybind-table maps)))

(defun general--print-keymap-heading (keymap-cons)
  "Print headings and tables for KEYMAP-CONS."
  (let ((keymap (car keymap-cons))
        (state-conses (cdr keymap-cons)))
    (princ (capitalize (concat "* " (symbol-name keymap) " Keybindings\n")))
    (let ((no-evil-state-cons (assq nil state-conses)))
      ;; non-evil keybindings go first
      (when no-evil-state-cons
        (general--print-state-heading no-evil-state-cons)
        (setq state-conses (assq-delete-all nil state-conses)))
      (dolist (state-cons state-conses)
        (general--print-state-heading state-cons)))))

;;;###autoload
(defun general-describe-keybindings ()
  "Show all keys that have been bound with general in an org buffer.
Global keybindings will be shown first. Currently, local keybindings are not
shown."
  (interactive)
  (with-output-to-temp-buffer "*General Keybindings*"
    (let* ((keybindings (copy-alist general-keybindings))
           (global-keybinds (assq 'global keybindings))
           (global-evil-keymaps
            '(evil-insert-state-map
              evil-emacs-state-map
              evil-normal-state-map
              evil-visual-state-map
              evil-motion-state-map
              evil-operators-state-map
              evil-outer-text-objects-map
              evil-inner-text-objects-map
              evil-replace-state-map
              evil-ex-search-keymap
              evil-ex-completion-map
              evil-command-window-mode-map
              evil-window-map))
           (global-evil-keybinds
            (cl-loop for keymap-cons in keybindings
                     when (memq (car keymap-cons) global-evil-keymaps)
                     append (list keymap-cons))))
      (when global-keybinds
        (general--print-keymap-heading global-keybinds)
        (setq keybindings (assq-delete-all 'global keybindings)))
      (when global-evil-keybinds
        (dolist (keymap-cons global-evil-keybinds)
          (general--print-keymap-heading keymap-cons)
          (setq keybindings (assq-delete-all (car keymap-cons) keybindings))))
      (dolist (keymap-cons keybindings)
        (general--print-keymap-heading keymap-cons))))
  (with-current-buffer "*General Keybindings*"
    (let ((org-startup-folded 'showall))
      (org-mode))
    (read-only-mode -1)
    (while (progn
             (while (progn
                      (forward-line)
                      (org-at-heading-p)))
             (org-table-align)
             (outline-next-heading)))
    (goto-char (point-min))
    (read-only-mode)))

;;; Commands that Could Aid in Key Definition
;; https://emacs.stackexchange.com/questions/6037/emacs-bind-key-to-prefix/13432#13432
;; altered to allow execution in a emacs state
;; and to create a named function with a docstring
;;;###autoload
(defmacro general-simulate-keys (keys &optional emacs-state docstring name)
  "Create a function to simulate KEYS.
If EMACS-STATE is non-nil, execute the keys in emacs state. Otherwise simulate
the keys in the current context (will work without evil). KEYS should be a
string  given in `kbd' notation. It an also be a list of a single command
followed by a string of the keys to simulate after calling that command. If
DOCSTRING is given, it will replace the automatically generated docstring. If
NAME is given, it will replace the automatically generated function name. NAME
should not be quoted."
  (let* ((command (when (listp keys)
                    (car keys)))
         (keys (if (listp keys)
                   (cadr keys)
                 keys))
         (name (or name
                   (intern (concat
                            (format "general-simulate-%s"
                                    (if command
                                        (eval command)
                                      ""))
                            (when command
                              "-")
                            (replace-regexp-in-string " " "_" keys)
                            (when emacs-state
                              "-in-emacs-state"))))))
    `(defun ,name
         ()
       ,(or docstring
            (concat "Simulate '" keys "' in " (if emacs-state
                                                  "emacs state."
                                                "the current context.")))
       (interactive)
       (when ,emacs-state
         ;; so don't have to redefine evil-stop-execute-in-emacs-state
         (setq this-command #'evil-execute-in-emacs-state)
         (let ((evil-no-display t))
           (evil-execute-in-emacs-state)))
       (let ((keys (kbd ,keys))
             (command ,command))
         (setq prefix-arg current-prefix-arg)
         (setq unread-command-events (listify-key-sequence keys))
         (when command
           (let ((this-command command))
             (call-interactively command)))))))

(defvar general--last-dispatch nil)

(defun general--fix-repeat (flag)
  "Modified version of `evil-repeat-keystrokes'.
It will remove extra keys added in a general-dispatch-... command."
  (eval-after-load 'evil
    '(cond ((eq flag 'pre)
            (when evil-this-register
              (evil-repeat-record
               `(set evil-this-register ,evil-this-register)))
            (setq evil-repeat-keys (this-command-keys)))
           ((eq flag 'post)
            (evil-repeat-record (if (zerop (length (this-command-keys)))
                                    evil-repeat-keys
                                  (this-command-keys)))
            (evil-clear-command-keys)
            (let* ((command (cl-getf general--last-dispatch :command))
                   (repeat-prop (evil-get-command-property command :repeat t))
                   (fallback (cl-getf general--last-dispatch :fallback))
                   (invoked-keys (cl-getf general--last-dispatch :invoked-keys))
                   (keys (cl-getf general--last-dispatch :keys)))
              (if (or (memq repeat-prop (list nil 'abort 'ignore))
                      (and (eq repeat-prop 'motion)
                           (not (memq evil-state '(insert replace)))))
                  (evil-repeat-abort)
                (if fallback
                    ;; may not know full key sequence
                    (setcar evil-repeat-info invoked-keys)
                  (setq evil-repeat-info
                        (list (concat invoked-keys keys))))))))))

;;;###autoload
(cl-defmacro general-key-dispatch
    (fallback-command &rest maps &key name docstring &allow-other-keys)
  "Create a function that will run FALLBACK-COMMAND or a command from MAPS.
MAPS consists of <key> <command> pairs. If a key in MAPS is matched, the
corresponding command will be run. Otherwise FALLBACK-COMMAND will be run
with the unmatched keys. So, for example, if \"ab\" was pressed, and \"ab\" is
not one of the key sequences from MAPS, the FALLBACK-COMMAND will be run
followed by the simulated keypresses of \"ab\". Prefix arguments will still work
regardless of which command is run. This is useful for binding under non-prefix
keys. For example, this can be used to redefine a sequence like \"cw\" or
\"cow\" in evil but still have \"c\" work as `evil-change'. LAMBDA, NAME, and
DOCSTRING are optional keyword arguments. They can be used to replace the
automatically generated name and docstring for the created function and are
potentially useful if you want to create multiple, different commands using the
same FALLBACK-COMMAND (e.g. `self-insert-command')."
  (declare (indent 1))
  (let ((name (or name (intern (format "general-dispatch-%s"
                                       (eval fallback-command)))))
        ;; remove keyword arguments from maps
        (maps (cl-loop for (key value) on maps by 'cddr
                       unless (keywordp key)
                       collect key
                       and collect value)))
    `(progn
       (when (fboundp 'evil-set-command-property)
         (evil-set-command-property #',name :repeat 'general--fix-repeat))
       (defun ,name (char)
         ,(or docstring (format "Run %s or something else based on CHAR."
                                (eval fallback-command)))
         ;; TODO maybe don't read in a char initially; rename char
         (interactive "c")
         (setq char (char-to-string char))
         (let ((map (make-sparse-keymap))
               ;; remove read in char
               (invoked-keys (substring (this-command-keys) 0 -1))
               matched-command
               fallback)
           (if general-implicit-kbd
               (general--emacs-define-key map
                 ,@(general--apply-prefix-and-kbd nil maps))
             (general--emacs-define-key map ,@maps))
           (while (keymapp (lookup-key map char))
             (setq char (concat char (char-to-string (read-char)))))
           (setq prefix-arg current-prefix-arg)
           (cond
            ((setq matched-command (lookup-key map char))
             ;; necessary for evil-this-operator checks because
             ;; evil-define-operator sets evil-this-operator to this-command
             (let ((this-command matched-command))
               (call-interactively matched-command)))
            (t
             (setq fallback t
                   matched-command ,fallback-command)
             ;; have to do this in "reverse" order (call command 2nd)
             (setq unread-command-events (listify-key-sequence char))
             (let ((this-command ,fallback-command))
               (call-interactively ,fallback-command))))
           (setq general--last-dispatch
                 `(:command ,matched-command
                            :invoked-keys ,invoked-keys
                            :keys ,char
                            :fallback ,fallback)))))))

;;; Optional Setup
;;;###autoload
(defmacro general-create-vim-definer
    (name keymaps &optional states default-to-states)
  "A helper function to create vim-like wrappers over `general-define-key'.
The function created will be called NAME and will have the keymaps default to
KEYMAPS or the states default to STATES. If DEFAULT-TO-STATES is non-nil,
:states STATES will be used. Otherwise :keymaps KEYMAPS will be used. This can
be overriden later by setting the global `general-vim-definer-default'
option."
  `(defun ,name (&rest args)
     ,(format "A wrapper function over `general-define-key'.

It has one the following defaults depending on `general-vim-definer-default':
:keymaps
%s

:states
%s

When `general-vim-definer-default' is nil, default to setting %s.
(If the default :states is nil, :keymaps will be used no matter what.)"
              keymaps states
              (if default-to-states
                  ":states"
                ":keymaps"))
     ;; can still override keywords afterwards
     (let ((default-to-states
             (cond ((eq general-vim-definer-default 'states)
                    t)
                   ((eq general-vim-definer-default 'keymaps)
                    nil)
                   (t
                    ,default-to-states))))
       (apply #'general-define-key
              (append args (if (and default-to-states ,states)
                               (list :states ,states)
                             (list :keymaps ,keymaps)))))))

;;;###autoload
(defmacro general-evil-setup (&optional short-names default-to-states)
  "Set up some basic equivalents for vim mapping functions.
This creates global key definition functions for the evil states.
Specifying SHORT-NAMES as non-nil will create non-prefixed function
aliases such as `nmap' for `general-nmap'."
  `(progn
     (general-create-vim-definer general-nmap
                                 'evil-normal-state-map 'normal
                                 ,default-to-states)
     (general-create-vim-definer general-imap
                                 'evil-insert-state-map 'insert
                                 ,default-to-states)
     (general-create-vim-definer general-vmap
                                 'evil-visual-state-map 'visual
                                 ,default-to-states)
     (general-create-vim-definer general-rmap
                                 'evil-replace-state-map 'replace
                                 ,default-to-states)
     (general-create-vim-definer general-omap
                                 'evil-operator-state-map 'operator
                                 ,default-to-states)
     (general-create-vim-definer general-mmap
                                 'evil-motion-state-map 'motion
                                 ,default-to-states)
     (general-create-vim-definer general-emap
                                 'evil-emacs-state-map 'emacs
                                 ,default-to-states)
     ;; these two don't have corresponding states
     (general-create-vim-definer general-otomap 'evil-outer-text-objects-map)
     (general-create-vim-definer general-itomap 'evil-inner-text-objects-map)
     (general-create-vim-definer general-nvmap
                                 '(evil-normal-state-map
                                   evil-visual-state-map)
                                 '(normal visual)
                                 ,default-to-states)
     (general-create-vim-definer general-nvmmap
                                 '(evil-normal-state-map
                                   evil-visual-state-map
                                   evil-motion-state-map)
                                 '(normal visual motion)
                                 ,default-to-states)
     (general-create-vim-definer general-iemap
                                 '(evil-insert-state-map
                                   evil-emacs-state-map)
                                 '(insert emacs)
                                 ,default-to-states)
     (general-create-definer general-tomap
                             :keymaps '(evil-outer-text-objects-map
                                        evil-inner-text-objects-map))
     (when ,short-names
       (defalias 'nmap #'general-nmap)
       (defalias 'imap #'general-imap)
       (defalias 'vmap #'general-vmap)
       (defalias 'rmap #'general-rmap)
       (defalias 'omap #'general-omap)
       (defalias 'emap #'general-emap)
       (defalias 'otomap #'general-otomap)
       (defalias 'itomap #'general-itomap)
       (defalias 'nvmap #'general-nvmap)
       (defalias 'nvmmap #'general-nvmmap)
       (defalias 'iemap #'general-iemap)
       (defalias 'tomap #'general-tomap))))

;;; Use-package Integration
(eval-after-load 'use-package
  (lambda ()
    (setq use-package-keywords
          ;; should go in the same location as :bind
          ;; adding to end may not cause problems, but see issue #22
          (cl-loop for item in use-package-keywords
                   if (eq item :bind-keymap*)
                   collect :bind-keymap* and collect :general
                   else
                   ;; don't add duplicates
                   unless (eq item :general)
                   collect item))
    (defun use-package-normalize/:general (_name _keyword args)
      args)
    (defun use-package-handler/:general (name _keyword arglists rest state)
      (let* ((sanitized-arglist
              ;; combine arglists into one without function names or
              ;; positional arguments
              (let (result
                    first)
                (dolist (arglist arglists result)
                  ;; either should work
                  ;; (while (and (setq first (car arglist))
                  ;;             (not (or (stringp first)
                  ;;                      (vectorp first)
                  ;;                      (and (listp first)
                  ;;                           (eq (car first) 'kbd)))))
                  ;;   (setq arglist (cdr arglist)))
                  (while (and (setq first (car arglist))
                              (or (and (symbolp first)
                                       (not (keywordp first)))
                                  (and (listp first)
                                       (eq (car first) 'quote))))
                    (setq arglist (cdr arglist)))
                  (setq result (append result arglist)))))
             (commands (cl-loop for (key command) on sanitized-arglist by 'cddr
                                unless (or (keywordp key)
                                           (not command))
                                ;; since :commands expects them unqouted
                                collect (eval command))))
        (use-package-concat
         (use-package-process-keywords name
           (use-package-sort-keywords
            (use-package-plist-maybe-put rest :defer t))
           (use-package-plist-append state :commands commands))
         `((ignore ,@(mapcar (lambda (arglist)
                               (let ((first (car arglist)))
                                 (if (and (symbolp first)
                                          (not (keywordp first)))
                                     `(,@arglist)
                                   `(general-define-key ,@arglist))))
                             arglists))))))))

(provide 'general)
;;; general.el ends here
