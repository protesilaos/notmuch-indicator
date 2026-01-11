;;; notmuch-indicator.el --- Display mode line indicator with notmuch-count(1) output -*- lexical-binding: t -*-

;; Copyright (C) 2022-2026  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/notmuch-indicator
;; Version: 1.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, mail

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is a simple package that renders an indicator with an email
;; count of the `notmuch' index on the Emacs mode line.  The
;; underlying mechanism is that of `notmuch-count(1)', which is used
;; to find the number of items that match the given search terms.  For
;; example, the letter "U" or the emoji "💬" can accompany search
;; terms for "unread items".  The resulting indicator may optionally
;; be styled with a face, only for the label, only for the counter, or
;; both.
;;
;; The indicator is displayed when `notmuch-indicator-mode' is enabled.
;;
;; The user option `notmuch-indicator-args' provides the means to define
;; search terms and associate them with a given label.  The label is
;; purely cosmetic, though it helps characterise the resulting counter.
;;
;; The user option `notmuch-indicator-refresh-count' determines how
;; often the indicator will be refreshed.  It accepts a numeric argument
;; which represents seconds.
;;
;; The user option `notmuch-indicator-force-refresh-commands' accepts as
;; its value a list of symbols.  Those are commands that will forcefully
;; update the indicator after they are invoked.
;;
;; The user option `notmuch-indicator-hide-empty-counters' hides zero
;; counters from the indicator, when it is set to a non-nil value.
;;
;; The user option `notmuch-indicator-add-to-mode-line-misc-info' can
;; be set to nil for those who want to control the placement of the
;; `notmuch-indicator-mode-line-construct'.
;;
;; Finally, and albeit obvious, the backronym for this package is:
;; notmuch-... Interested in Notmuch Data Indicators that Count Any
;; Terms Ordinarily Requested.

;;; Code:

(defgroup notmuch-indicator ()
  "Display mode line indicator with `notmuch-count(1)' output."
  :group 'notmuch)

;;;; User options

(defcustom notmuch-indicator-args '((:terms "tag:unread and tag:inbox" :label "@"))
  "List of plists specifying terms for `notmuch-count(1)'.

Each plist consists of one mandarory property and three optional
ones:

1. The `:terms', which is required, is a string that holds the
   command-line arguments passed to `notmuch-count(1)' (read the
   Notmuch documentation for the technicalities).

2. The `:label', which is optional, is an arbitrary string that
   is prepended to the return value of the above.  If the value
   is nil or the property is omitted, no label is displayed.

3. The `:label-face', which is optional, is the symbol of a face
   that is applied to the `:label'.  It should not be quoted, so
   like :face bold.  Good candidates are `bold', `italic',
   `success', `warning', `error', though anything will do.  If
   the value is nil or the property is omitted, no face is used.
   For backward-compatibility, `:face' has the same meaning as
   `:label-face'.

4. The `:counter-face', which is optional, is like `:label-face'
   but applies to the number of the given counter.  It accepts
   the unquoted symbol of a face, as noted above, though it also
   takes a `inherit' value which means to use the same face as
   the `:label-face'.  This too is an unquoted symbol.  If the
   value is nil, or the property is omitted altogether, the
   counter does not have a face assigned to it.

Multiple plist lists represent separate `notmuch-count(1)'
queries.  These are run sequentially.  Their return values are
joined into a list of strings.  By default, this is shown on the
mode line, wherever `mode-line-misc-info' is displayed.  Refer to
the user option `notmuch-indicator-add-to-mode-line-misc-info'
to control the placement of the `notmuch-indicator-mode-line-construct'.

For instance, a value like the following defines three
searches (in the source code the quotes are escaped---please
check the Help buffer for the clean code (I dislike markup in doc
strings)):

    (setq notmuch-indicator-args
         \\='((:terms \"tag:unread and tag:inbox\" :label \"@\")
            (:terms \"from:bank and tag:bills and tag:unpaid\" :label \"😱\")
            (:terms \"--output threads tag:fans\" :label \"❤️\")))

These form a list of strings which appears as something like
this: @2 😱1000 ❤️0 (sorry, the `notmuch-indicator' is no miracle
worker: your fan mail will not be more than your unpaid bills).

Same idea as above, but with faces applied:

    (setq notmuch-indicator-args
         \\='(( :terms \"tag:unread and tag:inbox\"
             :label \"@\"
             :label-face success)
           ( :terms \"from:bank and tag:bills and tag:unpaid\"
             :label \"😱\"
             :counter-face warning)
           ( :terms \"--output threads tag:fans\"
             :label \"❤️\"
             :label-face error
             :counter-face inherit)))"
  :type '(repeat
          (plist :options
                 (((const :tag "Search terms for `notmuch-count(1)'" :terms) string)
                  ((const :tag "Cosmetic label for the counter" :label) string)
                  ((const :tag "Face applied to the label" :label-face) face)
                  ((const :tag "Face applied to the counter" :counter-face) face))))
  :package-version '(notmuch-indicator . "1.2.0")
  :group 'notmuch-indicator)

(defcustom notmuch-indicator-hide-empty-counters nil
  "When non-nil, hide output of searches that have zero results."
  :type 'boolean
  :group 'notmuch-indicator)

;; TODO 2022-09-19: If this changes, the `notmuch-indicator-mode' needs
;; to be restarted.  We can add a custom setter here.  Perhaps there is
;; also some elegant way to handle this when the variable is changed
;; with `setq'.
(defcustom notmuch-indicator-refresh-count (* 60 3)
  "How often to update the indicator, in seconds.
It probably is better to not set this to a very low number.

Also see `notmuch-indicator-force-refresh-commands'."
  :type 'number
  :group 'notmuch-indicator)

(defcustom notmuch-indicator-force-refresh-commands '(notmuch-refresh-this-buffer)
  "List of commands that update the notmuch-indicator after invoked.
Normally, the indicator runs on a timer, controlled by the user
option `notmuch-indicator-refresh-count'."
  :type '(repeat function)
  :group 'notmuch-indicator)

(defun notmuch-indicator-get-config-file ()
  "Return `notmuch' configuration file."
  (catch 'found
    (dolist (path '("$XDG_CONFIG_HOME/notmuch/$NOTMUCH_PROFILE/config"
                    "$HOME/.config/notmuch/$NOTMUCH_PROFILE/config"
                    "$HOME/.config/notmuch/default/config"
                    "$HOME/.notmuch-config.$NOTMUCH_PROFILE"
                    "$HOME/.notmuch-config"))
      (when-let* ((config (substitute-env-vars path))
                  ((file-exists-p config)))
        (throw 'found config)))))

(defcustom notmuch-indicator-notmuch-config-file (notmuch-indicator-get-config-file)
  "File system path to the local user's Notmuch configuration file.

The file is one among:

- $XDG_CONFIG_HOME/notmuch/$NOTMUCH_PROFILE/config
- $HOME/.notmuch-config.$NOTMUCH_PROFILE
- $HOME/.notmuch-config

See the function `notmuch-indicator-get-config-file' for how we
return the right path.  The user can set this user option to an
arbitrary path if the aforementioned function does not return the
desired value.

We store this the first time `notmuch-indicator-mode' is loaded
so that the indicator can still show the data of the local user
even when they are browsing a remote file system with TRAMP.
Alternatively, the user may wish to have different indicators
depending on the TRAMP environment, in which case the value of
this user option must be updated accordingly (DEV NOTE: please
contact me if you have such a use-case, as I am happy to make the
package more flexible)."
  :type 'file
  :package-version '(notmuch-indicator . "1.1.0")
  :group 'notmuch-indicator)

(defcustom notmuch-indicator-notmuch-binary (executable-find "notmuch")
  "File system path to the `notmuch' binary."
  :type 'file
  :package-version '(notmuch-indicator . "1.1.0")
  :group 'notmuch-indicator)

(defcustom notmuch-indicator-add-to-mode-line-misc-info t
  "When non-nil, append the notmuch indicator to the mode line.
Experienced users can set this to a nil value and then include
the `notmuch-indicator-mode-line-construct' anywhere they want in
`mode-line-format' or related.  Also read the manual for an
example that uses the `tab-bar-mode'."
  :type 'boolean
  :package-version '(notmuch-indicator . "1.2.0")
  :group 'notmuch-indicator)

;;;; Helper functions and the minor-mode

(defun notmuch-indicator--shell-command (terms)
  "Run shell command for `notmuch-count(1)' with TERMS."
  (replace-regexp-in-string
   "\n" ""
   (let ((default-directory "~"))
     (shell-command-to-string
      (format "%s --config=%S count %s"
              notmuch-indicator-notmuch-binary
              notmuch-indicator-notmuch-config-file
              (shell-quote-argument terms))))))

(declare-function
 notmuch-search "notmuch"
 (&optional query oldest-first target-thread target-line no-display))

(defvar notmuch-indicator-counter-format "%s%s"
  "The `format' string for each counter.
It accepts two %s specifiers for the label and number,
respectively.")

(defun notmuch-indicator--format-label (label count label-face counter-face terms)
  "Format `notmuch-indicator-args'.
LABEL is the value of the `:label' property.  If nil, return an
empty string.

COUNT is the return value of the search TERMS of property
`:terms'.  If nil, return an empty string.

LABEL-FACE is the value of the `:label-face' property, while
COUNTER-FACE is that of `:counter-face'.  Apply them to LABEL and
COUNT, respectively.  If nil, do not propertize LABEL or COUNT
with a face."
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
                (lambda () (interactive) (notmuch-search terms)))
    (concat " " ; to separate multiple counters without changing the mouse hover highlight
            (propertize
             (format notmuch-indicator-counter-format
                     (if (and label-face label)
                         (propertize label 'face label-face)
                       (or label ""))
                     (if (and counter-face count)
                         (propertize count 'face counter-face)
                       (or count "")))
             'mouse-face 'mode-line-highlight
             'help-echo (format "mouse-1: Open notmuch search for `%s'" terms)
             'local-map map))))

(defun notmuch-indicator--get-counter-face (properties)
  "Get :counter-face from PROPERTIES.
If its value is `inherit', get the `:label-face'."
  (let ((value (plist-get properties :counter-face)))
    (if (eq value 'inherit)
        (or (plist-get properties :label-face)
            (plist-get properties :face))
      value)))

(defun notmuch-indicator--format-counter (count properties)
  "Format counter with COUNT and PROPERTIES of `notmuch-indicator-args'."
  (notmuch-indicator--format-label
   (plist-get properties :label)
   count
   (or (plist-get properties :label-face) (plist-get properties :face))
   (notmuch-indicator--get-counter-face properties)
   (plist-get properties :terms)))

(defun notmuch-indicator--get-counters ()
  "Return `notmuch-indicator-args' per `notmuch-indicator-hide-empty-counters'."
  (delq nil
        (mapcar
         (lambda (properties)
           (let ((count (notmuch-indicator--shell-command (plist-get properties :terms))))
             (unless (and (zerop (string-to-number count))
                          notmuch-indicator-hide-empty-counters)
               (notmuch-indicator--format-counter count properties))))
         notmuch-indicator-args)))

(defvar notmuch-indicator--counters nil
  "Store the return value of `notmuch-indicator--indicator'.")

(defun notmuch-indicator--indicator ()
  "Return contents of mode line indicator."
  (setq notmuch-indicator--counters
        (or (notmuch-indicator--get-counters) "")))

(defvar-local notmuch-indicator-mode-line-construct
    '(notmuch-indicator-mode (" " (:eval notmuch-indicator--counters)))
  "Show the notmuch-indicator on the mode line.
Do it when `notmuch-indicator-mode' is enabled.  Also see
`notmuch-indicator-add-to-mode-line-misc-info'.")

(put 'notmuch-indicator-mode-line-construct 'risky-local-variable t)

(defun notmuch-indicator-tab-bar-format ()
  "Notmuch indicator construct suitable for `tab-bar-format'."
  `((global menu-item ,(format-mode-line notmuch-indicator-mode-line-construct) ignore)))

(defun notmuch-indicator--running-p ()
  "Return non-nil if `notmuch-indicator--indicator' is running."
  (when (and notmuch-indicator-notmuch-config-file notmuch-indicator-notmuch-binary)
    (delq nil
          (mapcar
           (lambda (timer)
             (eq (timer--function timer) 'notmuch-indicator--indicator))
           timer-list))))

(defun notmuch-indicator--run ()
  "Run the timer with a delay, starting it if necessary.
The delay is specified by `notmuch-indicator-refresh-count'."
  (unless (notmuch-indicator--running-p)
    (run-at-time nil notmuch-indicator-refresh-count #'notmuch-indicator--indicator)))

(defun notmuch-indicator-refresh ()
  "Refresh the active indicator."
  (when (notmuch-indicator--running-p)
    (cancel-function-timers #'notmuch-indicator--indicator)
    (run-at-time nil notmuch-indicator-refresh-count #'notmuch-indicator--indicator)))

(define-obsolete-function-alias
  'notmuch-indicator--refresh
  'notmuch-indicator-refresh
  "0.3.0")

(defvar notmuch-indicator--used-mode-line-construct nil
  "Mode line construct last added by `notmuch-indicator-mode'.")

;;;###autoload
(define-minor-mode notmuch-indicator-mode
  "Display mode line indicator with `notmuch-count(1)' output.

For the search terms and the label that can accompany them, refer
to the user option `notmuch-indicator-args'.

To control how often the indicator is updated, check the user
option `notmuch-indicator-refresh-count'.."
  :init-value nil
  :global t
  (if notmuch-indicator-mode
      (progn
        (when notmuch-indicator-add-to-mode-line-misc-info
          (setq notmuch-indicator--used-mode-line-construct notmuch-indicator-mode-line-construct)
          (add-to-list 'mode-line-misc-info notmuch-indicator-mode-line-construct))
        (notmuch-indicator--run)
        (dolist (fn notmuch-indicator-force-refresh-commands)
          (advice-add fn :after #'notmuch-indicator-refresh)))
    (setq mode-line-misc-info (delete notmuch-indicator--used-mode-line-construct mode-line-misc-info))
    (cancel-function-timers #'notmuch-indicator--indicator)
    (dolist (fn notmuch-indicator-force-refresh-commands)
      (advice-remove fn #'notmuch-indicator-refresh))
    (force-mode-line-update t)))

(provide 'notmuch-indicator)
;;; notmuch-indicator.el ends here
