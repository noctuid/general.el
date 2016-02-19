;;; general.el --- Convenience wrappers for keybindings.

;; Author: Fox Kiester <noct@openmailbox.org>
;; URL: https://github.com/noctuid/general.el
;; Created: February 17, 2016
;; Keywords: vim, evil, leader, keybindings, keys
;; Package-Requires: ((dash "2.11.0") (cl-lib "0.5"))
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
(require 'dash)
(require 'cl-lib)

(defgroup general nil
  "Gives convenient wrappers for key definitions."
  :group 'convenience
  :prefix 'general)

(defcustom general-implicit-kbd t
  "Whether to implicitly wrap a (kbd) around keybindings.
This applies to the prefix key as well."
  :group 'general
  :type 'boolean)

(defcustom general-default-prefix nil
  "The default prefix key sequence to use."
  :group 'general
  :type 'string)

(defcustom general-default-state nil
  "The default evil state to make mappings in.
Non-evil users should keep this nil."
  :group 'general
  :type '(choice
          (const :tag "Normal state" normal)
          (const :tag "Insert state" insert)
          (const :tag "Visual state" visual)
          (const :tag "Replace state" replace)
          (const :tag "Operator state" operator)
          (const :tag "Motion state" motion)
          (const :tag "Emacs state" emacs)
          (const :tag "Use define-key not evil-define-key" nil)))

(defcustom general-default-keymap global-map
  "The default keymap to bind keys in."
  :group 'general)

(defun general--apply-prefix (prefix maps)
  "Prepend the PREFIX sequence to all MAPS.
Adds a (kbd ...) if `general-implicit-kbd' is non-nil."
  (let ((prefix (or prefix "")))
    (--map-when (stringp it)
                (if general-implicit-kbd
                    (kbd (concat prefix " " it))
                  (concat prefix it))
                maps)))

;;; define-key and evil-define-key Wrappers
;; TODO better way to do this?
;; https://www.reddit.com/r/emacs/comments/1582uo/bufferlocalsetkey_set_a_key_in_one_buffer_only/
(defun general--emacs-local-set-key (key func)
  "Bind KEY to FUNC for the current buffer only using a minor mode."
  (let* ((mode-name-loc (gensym "general-blm")))
    (eval `(define-minor-mode ,mode-name-loc nil nil nil (make-sparse-keymap)))
    (setq buffer-local-mode mode-name-loc)
    (funcall mode-name-loc 1)
    (define-key
      (symbol-value (intern (concat (symbol-name mode-name-loc) "-map")))
      key func)))

;; this works but will make it so that keys defined for the major mode will no longer affect
;; (use-local-map (copy-keymap (current-local-map)))
;; (local-set-key (kbd "C-c y") 'helm-apropos)

(defun general--emacs-define-key (prefix keymap &rest maps)
  "A more convenient way to define keys in general.
This function is only meant to be used as a wrapper for `define-key' for those
who do not wish to use the generic `general-define-key'. Unlike the generic
function, positional arguments are required. PREFIX corresponds to a prefix
sequence of any length. KEYMAP determines the keymap to bind the MAPS in."
  (declare (indent defun))
  (setq maps (general--apply-prefix prefix maps))
  (let (key func)
    (while (setq key (pop maps)
                 func (pop maps))
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

(defun general--evil-define-key (prefix state keymap &rest maps)
  "A more convenient way to define keys for evil.
This function is only meant to be used as a wrapper for `evil-define-key' for
those who do not wish to use the generic `general-define-key'. Unlike the
generic function, positional arguments are required. PREFIX corresponds to a
prefix sequence of any length. STATE corresponds to the evil state to make the
keybindings in. KEYMAP determines the keymap to bind the MAPS in."
  (declare (indent 3))
  (setq maps (general--apply-prefix prefix maps))
  (eval-after-load 'evil
    (let (key func)
      (while (setq key (pop maps)
                   func (pop maps))
        (if (eq keymap 'local)
            ;; has no &rest
            (evil-local-set-key state key func)
          (evil-define-key state keymap key func))))))
    

;;; Functions With Keyword Arguments
;; TODO does autoload recognize cl-defun?
;;;###autoload
(cl-defun general-define-key
    (&rest maps &key (prefix general-default-prefix)
           (state general-default-state)
           (keymap general-default-keymap)
           &allow-other-keys)
  "The primary key definition function provided by general.
PREFIX corresponds to a prefix key and defaults to none. STATE corresponds to
the evil state to bind the keys in. Non-evil users should not set STATE. When
STATE is non-nil, `evil-define-key' will be used. Otherwise `define-key' will be
used. Evil users may also want to leave STATE nil and set KEYMAP to
`evil-normal-state-map', for example, for global keybindings. KEYMAP defaults
to `global-map'."
  ;; remove keyword arguments from rest var
  (setq maps
        (cl-loop for (key value) on maps by 'cddr
                 when (not (member key (list :prefix :state :keymap)))
                 collect key
                 and collect value))
  (if state
      (apply #'general--evil-define-key prefix state keymap maps)
    (apply #'general--emacs-define-key prefix keymap maps)))

;;;###autoload
(defmacro general-create-definer (name &rest args)
  "A helper macro to create key definitions functions.
This allows the creation of key definition functions that
will use a certain keymap, evil state, and/or prefix key by default.
NAME will be the function name and ARGS are the keyword arguments that
are intended to be the defaults."
  `(defun ,name (&rest args)
     ;; can still override keywords afterwards
     (apply #'general-define-key ,@args args)))

;;; Positional Non-cl-lib Functions
;;;###autoload
(defun general-define-key-positional (prefix state keymap &rest maps)
  "A key definition function that uses positional arguments.
This is meant to be used by those who do not wish to use cl-lib or have keyword
arguments. It is not recommended. See the docstring of `general-define-key' for
information about PREFIX, STATE, KEYMAP, and MAPS."
  (if state
      (apply #'general--evil-define-key prefix state keymap maps)
    (apply #'general--emacs-define-key prefix keymap maps)))

;;;###autoload
(defmacro general-create-positional-definer
    (name &optional prefix state keymap)
  "A helper macro to create key definitions functions without support for
keyword arguments. This allows the creation of key definition functions that
will use a certain keymap, evil state, and/or prefix key by default.
NAME is the name the function will be given. PREFIX, STATE, and KEYMAP will be
the new defaults."
  (let ((arg-list (list '&rest 'maps))
        indent-num)
    (unless keymap (setq arg-list (cons 'keymap arg-list)))
    (unless state (setq arg-list (cons 'state arg-list)))
    (unless prefix (setq arg-list (cons 'prefix arg-list)))
    (setq indent-num (- (--reduce-from (if it (+ acc 1) acc) 0 arg-list)
                        2))
    `(defun ,name ,arg-list
       (declare (indent ,indent-num))
       (let ((prefix (or ,prefix prefix))
             (state (or ,state state))
             (keymap (or ,keymap keymap)))
         (apply #'general-define-key-positional prefix state keymap maps)))))

;;; Optional Setup
;;;###autoload
(defun general-evil-setup (&optional short-names)
  "Set up some basic equivalents for vim mapping functions.
This creates global key definition functions for the evil states.
Specifying SHORT-NAMES as non-nil will create non-prefixed function
aliases such as `nmap' for `general-nmap'."
  (require 'evil)
  (general-create-definer general-nmap :keymap evil-normal-state-map)
  (general-create-definer general-imap :keymap evil-insert-state-map)
  (general-create-definer general-vmap :keymap evil-visual-state-map)
  (general-create-definer general-rmap :keymap evil-replace-state-map)
  (general-create-definer general-omap :keymap evil-operator-state-map)
  (general-create-definer general-mmap :keymap evil-motion-state-map)
  (general-create-definer general-emap :keymap evil-emacs-state-map)
  (when short-names
    (defalias 'nmap 'general-nmap)
    (defalias 'imap 'general-imap)
    (defalias 'vmap 'general-vmap)
    (defalias 'rmap 'general-rmap)
    (defalias 'omap 'general-omap)
    (defalias 'emap 'general-emap)))

(provide 'general)
;;; general.el ends here
