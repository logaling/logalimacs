;;; logalimacs.el --- Front-end to logaling-command for Ruby gems

;; Copyright (C) 2011, 2012 by Yuta Yamada

;; Author: Yuta Yamada <yamada@clear-code.com>
;; URL: https://github.com/logaling/logalimacs
;; Version: 0.0.2

;;; Licence:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This is Front-end to logaling-command for Ruby gems.
;; Logalimacs.el lookup to registered term at logaling-command and,
;; Executes other commands for logaling-command from emacs.

;;; convenience configuration for popwin:
;;;###autoload (when (require 'popwin nil t) (defvar display-buffer-function 'popwin:display-buffer) (defvar popwin:special-display-config (append '(("*logalimacs*" :position top :height 10 :noselect t :stick t))) popwin:special-display-config))

(defvar loga-fly-mode nil)
(defvar loga-make-buffer "*logalimacs*" "display buffer name.")
(defvar loga-log-output nil "if nonnil, output log for developer.")
(defvar loga-fly-mode-interval 1
  "timer-valiable for loga-fly-mode, credit par sec.")

(defvar loga-command-alist
  '((?a . "add")
    (?c . "config")
    (?d . "delete")
    (?h . "help")
    (?i . "import")
    (?l . "lookup")
    (?n . "new")
    (?r . "register")
    (?U . "unregister")
    (?u . "update")
    (?v . "version")
    (?f . "loga-fly-mode")))

;;;###autoload
(defun loga-interactive-command ()
  "interactive-command for logaling-command, types following mini-buffer."
  (interactive)
  (let* (task)
    (save-current-buffer
    (read-event "types prefix of feature that want you :\n a)dd,c)onfig,d)elete,h)elp,i)mport,l)ookup,n)ew,r)egister,U)nregister,u)pdate,v)ersion,f)ly-mode")
    (setq task (assoc-default last-input-event loga-command-alist))
    (unless (equal task "loga-fly-mode")
      (logaling-command "help" task))
    (cond ((equal task "add") (loga-add-word))
          ((equal task "lookup") (loga-lookup-region-or-manually))
          ((equal task "config")
           (logaling-command task (read-string "loga config: ")))
          ((equal task "delete")
           (logaling-command task (read-string "loga delete: ")))
          ((equal task "help")
           (logaling-command task (read-string "loga help: ")))
          ((equal task "import")
           (logaling-command task (read-string "loga import: ")))
          ((equal task "new")
           (logaling-command task (read-string "loga new: ")))
          ((equal task "register")
           (logaling-command task (read-string "loga register: ")))
          ((equal task "unregister")
           (logaling-command task (read-string "loga unregister: ")))
          ((equal task "update") (loga-update))
          ((equal task "version") (logaling-command task))
          ((equal task "loga-fly-mode") (loga-fly-mode))))))

;; @todo apply ansi-color
(defun loga-to-shell (cmd &optional arg)
  (ansi-color-apply (shell-command-to-string (concat cmd " " arg " &"))))

(defun logaling-command (task &optional arg output)
  (let*
      ((content (loga-to-shell (concat "\\loga " task) arg)))
    (cond ((eq output :popup) (loga-make-popup content))
          (t (loga-make-buffer content)))))

(defun loga-make-buffer(content)
  "create buffer for logalimacs"
  (let* ((buff (symbol-value 'loga-make-buffer)))
    (save-current-buffer
      (save-selected-window
        (with-current-buffer
            (switch-to-buffer-other-window (get-buffer-create buff))
          (erase-buffer) ;;initialize
          (insert content)
          (beginning-of-buffer))))))

(defun loga-make-popup (content)
  (if (require 'popup nil t)
      (save-current-buffer
        (popup-tip content :scroll-bar t))
    (print "can't lookup, it is require popup.el.")))

;;;###autoload
(defun loga-add-word ()
  "this is command to adding word, first source word, second target word."
  (interactive)
  (let*
      ((source (loga-return-region-or-wait-for-key-in "adding word here: "))
       (target (read-string "translated word here: "))
       (note (read-string "annotation here(optional): "))
       (sep "\" \""))
    (logaling-command "add"
                      (concat "\"" source sep target sep note "\""))))
;;;###autoload
(defun loga-update ()
  "update to registered word"
  (interactive)
  (let*
      ((src (loga-return-region-or-wait-for-key-in "source word here: "))
       (old (read-string "old target here: "))
       (new (read-string "new target here: "))
       (note (read-string "annotation here(optional): "))
       (sep "\" \""))
    (logaling-command "update"
                      (concat "\"" src sep old sep new sep note "\""))))

;;;###autoload
(defun loga-lookup-region-or-manually (&optional word-for-fly-mode)
  "search word from logaling. if not mark region, search word type on manual. otherwise passed character inside region."
  (interactive)
  (let* ((word (or word-for-fly-mode
                   (loga-return-region-or-wait-for-key-in "Search word here: "))))
    (save-current-buffer
      (logaling-command "lookup" word))))

;;;###autoload
(defun loga-lookup-for-popup ()
  "Display the output of loga-lookup at tooltip, note require popup.el"
  (interactive)
  (let*
      ((word (concat "\"" (loga-return-region-or-cursor) "\"")))
    (logaling-command "lookup" word :popup)))

(defun loga-return-region-or-wait-for-key-in (&optional prompt)
  "If mark is active, return the region, otherwise, read string with PROMPT."
  (or (loga-return-string-of-region)
      (read-string (or prompt "types here: "))))

(defun loga-return-region-or-cursor ()
  (or (loga-return-string-of-region)
      (loga-return-word-on-cursor)))

(defun loga-return-string-of-region ()
  "If active region, return it string. otherwise return nil."
  (interactive)
  (if mark-active
      (buffer-substring-no-properties (region-beginning) (region-end))
    nil))

(defun loga-return-word-on-cursor ()
  "return word where point on cursor"
  (let* (match-word)
    (save-excursion
      (backward-char)
      (cond
       ((not (looking-at "\\w"))
        (forward-char)
        (looking-at "\\w+") t)
       (t
        (forward-char)
        (backward-word)
        (looking-at "\\w+")))
      (setq match-word (match-string 0))
      (if loga-log-output (print match-word)) ;;log
      match-word)))

;;;###autoload
(defun loga-fly-mode ()
  "toggle loga-fly-mode-on and loga-fly-mode-off"
  (interactive)
  (if (symbol-value 'loga-fly-mode)
      (loga-fly-mode-off)
    (loga-fly-mode-on)))

(defun loga-fly-mode-on ()
  (setq loga-fly-mode t
        loga-fly-timer
        (run-with-idle-timer (symbol-value 'loga-fly-mode-interval) t
            (lambda()
             (let* ((fly-word (loga-return-word-on-cursor)))
               (if fly-word
                   (loga-lookup-region-or-manually fly-word))))))
  (message "loga-fly-mode enable"))

(defun loga-fly-mode-off ()
  (cancel-timer loga-fly-timer)
  (setq loga-fly-mode nil)
  (message "loga-fly-mode disable"))

;;;###autoload
(defun loga-get-flymake-error ()
  (interactive)
  (let* ((line-no            (flymake-current-line-no))
         (line-err-info-list (nth 0 (flymake-find-err-info flymake-err-info
                                                           line-no)))
         (count              (length line-err-info-list)))
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (flymake-ler-file (nth (1- count)
                                                  line-err-info-list)))
               (full-file (flymake-ler-full-file (nth (1- count)
                                                      line-err-info-list)))
               (text      (flymake-ler-text (nth (1- count)
                                                 line-err-info-list)))
               (line      (flymake-ler-line (nth (1- count)
                                                 line-err-info-list))))
          (loga-make-buffer (format "[%s] %s" line text))))
      (setq count (1- count)))))

(provide 'logalimacs)
;;; logalimacs.el ends here
