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

;;; keybins:
;;;###autoload (global-set-key (kbd "M-g M-u") 'loga-lookup-in-hand-or-region)
;;;###autoload (global-set-key (kbd "M-g M-a") 'loga-add-word)
;;;###autoload (global-set-key (kbd "M-g M-i") 'loga-interactive-command)

;;; convenience configuration for popwin:
;;;###autoload (when (require 'popwin nil t) (defvar display-buffer-function 'popwin:display-buffer) (defvar popwin:special-display-config (append '(("*logalimacs*" :position top :height 10 :noselect t :stick t))) popwin:special-display-config))

(defvar loga-fly-mode nil)
(defvar loga-make-buffer "*logalimacs*")
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
      (loga-prompt-command "help" task t))
    (cond ((equal task "add") (loga-add-word))
          ((equal task "lookup") (loga-lookup-in-hand-or-region))
          ((equal task "config")
           (loga-prompt-command task (read-string "loga config: ")))
          ((equal task "delete")
           (loga-prompt-command task (read-string "loga delete: ")))
          ((equal task "help")
            (loga-prompt-command task (read-string "loga help: ")))
          ((equal task "import")
            (loga-prompt-command task (read-string "loga import: ")))
          ((equal task "new")
           (loga-prompt-command task (read-string "loga new: ")))
          ((equal task "register")
           (loga-prompt-command task (read-string "loga register: ")))
          ((equal task "unregister")
           (loga-prompt-command task (read-string "loga unregister: ")))
          ((equal task "update") (loga-update))
          ((equal task "version") (loga-prompt-command task))
          ((equal task "loga-fly-mode") (loga-fly-mode))))))

(defun loga-prompt-command (task &optional arg help ext)
  "this function is wrapped program that pass to shell-command"
  (let* ((to-shell '(lambda ()
                      (shell-command-to-string
                       (if ext
                           (concat task " " arg " &")
                         (concat "\\loga " task " " arg (unless help " &")))))))
    (loga-make-buffer (funcall to-shell))))

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

;;;###autoload
(defun loga-add-word ()
  "this is command to adding word, first source word, second target word."
  (interactive)
  (let*
      ((source (loga-point-or-read-string "adding word here: "))
       (target (read-string "translated word here: "))
       (note (read-string "annotation here(optional): "))
       (sep "\" \""))
    (loga-prompt-command "add"
                         (concat "\"" source sep target sep note "\""))))
;;;###autoload
(defun loga-update ()
  "update to registered word"
  (interactive)
  (let*
      ((src (loga-point-or-read-string "source word here: "))
       (old (read-string "old target here: "))
       (new (read-string "new target here: "))
       (note (read-string "annotation here(optional): "))
       (sep "\" \""))
    (loga-prompt-command "update"
                         (concat "\"" src sep old sep new sep note "\""))))

;;;###autoload
(defun loga-lookup-in-hand-or-region (&optional word-for-fly-mode)
  "search word from logaling. if not mark region, search word type on manual. otherwise passed character inside region."
  (interactive)
  (let* ((word (or word-for-fly-mode
                   (loga-point-or-read-string "Search word here: "))))
    (save-current-buffer
      (loga-prompt-command "lookup" word))))

(defun loga-point-or-read-string (&optional prompt no-region)
  "If mark is active, return the region, otherwise, read string with PROMPT."
  (cond
   ((and mark-active (not no-region))
    (buffer-substring-no-properties (region-beginning) (region-end)))
   (t
    (read-string (or prompt "types here: ")))))

(defun loga-return-word-on-cursor ()
  "return word where point on cursor"
  (let* (match-word)
    (save-excursion
      (backward-char)
      (if (looking-at "[ \t\-]")
          (looking-at "\\w+")
        (forward-char)
        (backward-word)
        (looking-at "\\w+"))
      (setq match-word (match-string 0))
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
        (run-with-idle-timer 1 t
            (lambda()
             (let* ((fly-word (loga-return-word-on-cursor)))
               (if fly-word
                   (loga-lookup-in-hand-or-region fly-word))))))
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
