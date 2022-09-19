;;; notmuch-indicator.el --- Add notmuch count to the global-mode-string (mode line) -*- lexical-binding: t -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: THIS-IS-A-SAMPLE Development <~protesilaos/THIS-IS-A-SAMPLE@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/notmuch-indicator
;; Mailing-List: https://lists.sr.ht/~protesilaos/THIS-IS-A-SAMPLE
;; Version: 0.0.0
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
;; Work-in-progress.

;;; Code:

(defgroup notmuch-indicator ()
  "WORK-IN-PROGRESS."
  :group 'notmuch)

;; TODO 2022-09-19: How do we actually use 'notmuch count --batch'?
(defcustom notmuch-indicator-args
  '(("tag:unread and tag:inbox" "@"))
  "Arguments to format the notmuch mail indicator.

Each list consists of two strings:

1. The command-line arguments passed to notmuch count.
2. A string that is prepended to the return value of the above.

Multiple lists represent separate notmuch count queries.  These
are run sequentially.  Their return values are joined into a
single string.

For instance, a value like the following specifies two commands:

    (setq notmuch-indicator-args
          \='((\"tag:unread and tag:inbox\" \"@\")
            (\"--output threads from:VIP\" \"🤡\")))

These form a string like: @50 🤡10."
  :type '(repeat (list string))
  :group 'notmuch-indicator)

;; TODO 2022-09-19: If this changes, the `notmuch-indicator-mode' needs
;; to be restarted.  We can add a custom setter here.  Perhaps there is
;; also some elegant way to handle this when the variable is changed
;; with `setq'.
(defcustom notmuch-indicator-refresh-count (* 60 3)
  "How often to update the indicator, in seconds."
  :type 'number)

(defun notmuch-indicator--return-count ()
  "Parse `notmuch-indicator-args' and format them as single string."
  (mapconcat
   (lambda (s)
     (format "%s%s" (or (cadr s) "")
             (replace-regexp-in-string
              "\n" " "
              (shell-command-to-string
               (format "notmuch count %s" (car s))))))
     notmuch-indicator-args
     " "))

(defvar notmuch-indicator--last-state nil
  "Internal variable used to store the indicator's state.")

(defun notmuch-indicator--indicator ()
  "Prepare new mail count mode line indicator."
  (let* ((count (concat (notmuch-indicator--return-count) " "))
         (old-indicator notmuch-indicator--last-state))
    (when old-indicator
      (setq global-mode-string (delete old-indicator global-mode-string)))
    (cond
     (count
      (setq global-mode-string (push count global-mode-string))
      (setq notmuch-indicator--last-state count))
     (t
      (setq notmuch-indicator--last-state nil))))
  (force-mode-line-update t))

(defun notmuch-indicator--running-p ()
  "Return non-nil if `notmuch-indicator--indicator' is running."
  (delq nil
        (mapcar (lambda (timer)
                  (eq (timer--function timer) 'notmuch-indicator--indicator))
                timer-list)))

(defun notmuch-indicator--run ()
  "Run the timer with a delay, starting it if necessary.
The delay is specified by `notmuch-indicator-refresh-count'."
  (unless (notmuch-indicator--running-p)
    (notmuch-indicator--indicator))
  (run-at-time t notmuch-indicator-refresh-count #'notmuch-indicator--indicator))

;;;###autoload
(define-minor-mode notmuch-indicator-mode
  "Enable with counter for new mail.

If the `global-mode-string' is displayed on the `tab-bar-mode',
there may be a slight delay until the information is updated."
  :init-value nil
  :global t
  (if notmuch-indicator-mode
      (notmuch-indicator--run)
    (cancel-function-timers #'notmuch-indicator--indicator)
    (setq global-mode-string (delete notmuch-indicator--last-state global-mode-string))
    (force-mode-line-update t)))

(provide 'notmuch-indicator)
;;; notmuch-indicator.el ends here
