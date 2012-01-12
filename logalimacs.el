;;; This is front-end program for logaling-command
;;;
;;; Copyright (C) 2011  yuta yamada <yamada@clear-code.com>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

(defun loga-prompt-command (cmd &optional arg help)
  "this function is wrapped program that pass to shell-command"
  (let* ((to-shell '(lambda ()
                      (shell-command-to-string
                       (concat "\\loga " cmd " " arg (unless help " &"))))))
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

(defun loga-update ()
  "update to registered word"
  (let*
      ((src (loga-point-or-read-string "source word here: "))
       (old (read-string "old target here: "))
       (new (read-string "new target here: "))
       (note (read-string "annotation here(optional): "))
       (sep "\" \""))
    (loga-prompt-command "update"
                         (concat "\"" src sep old sep new sep note "\""))))

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
  (interactive)
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

(provide 'logalimacs)
