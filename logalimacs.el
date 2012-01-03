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
    ))

(defun loga-interactive-command ()
  "interactive-command for logaling-command, types following mini-buffer."
  (interactive)
  (let* (task)
    (save-current-buffer
    (read-event "types prefix of feature that want you :\n a)dd,c)onfig,d)elete,h)elp,i)mport,l)ookup,n)ew,r)egister,U)nregister,u)pdate,v)ersion")
    (setq task (assoc-default last-input-event loga-command-alist))
    (loga-prompt-command "help" task t)
    (cond ((equal task "add") (loga-add-word))
          ((equal task "lookup") (loga-lookup-in-hand-or-region))
          ((equal task "config")
           (loga-prompt-command task (read-string "loga config: ")))
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
          ((equal task "update")
           (loga-prompt-command task (read-string "loga update: ")))
          ((equal task "version")
           (loga-prompt-command task))
          ))
    ))

(defun loga-prompt-command (cmd &optional arg help)
  "this function is wrapped program that pass to shell-command"
  (let* ((logabuff "*logalimacs*"))
    (save-current-buffer
      (save-selected-window
        (with-current-buffer (get-buffer-create logabuff))
        (pop-to-buffer (get-buffer logabuff))
        (switch-to-buffer-other-window logabuff)
        (if help
            (shell-command (concat "\\loga " cmd " " arg) logabuff)
          (shell-command (concat "\\loga " cmd " " arg " &") logabuff)
          )
        ))))

(defun loga-add-word ()
  "this is command to adding word, first source word, second target word."
  (interactive)
  (let*
      ((source (loga-point-or-read-string "adding word here: "))
       (target (read-string "translated word here: "))
       (note (read-string "annotation here(optional): "))
       )
    (loga-prompt-command "add" (concat source " " target " " note))
    )
  )

(defun loga-lookup-in-hand-or-region ()
  "search word from logaling. if not mark region, search word type on manual. otherwise passed character inside region."
  (interactive)
  (let* ((word (loga-point-or-read-string "Search word here: ")))
    (save-current-buffer
      (loga-prompt-command "lookup" word)
      )))

(defun loga-point-or-read-string (&optional prompt no-region)
  "If mark is active, return the region, otherwise, read string with PROMPT."
  (cond
   ((and mark-active (not no-region))
    (buffer-substring-no-properties (region-beginning) (region-end)))
   (t
    (read-string (or prompt "types here: ")))))

(defun loga-return-word-of-cursor()
  "return word where point of cursor"
  (let* (match-word)
    (save-excursion
      (if (looking-at "[\\w ]")
          (backward-word)
        )
      (looking-at "\\w+")
      (setq match-word (match-string 0))
      match-word
      )))

(provide 'logalimacs)
