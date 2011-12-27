;;; this is wrapped program for logaling-command
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

(defun loga-logaling-prompt-command (cmd arg &optional nobuffer)
  "yet can't be docstring"
  (interactive)
  (shell-command (concat "\\loga " cmd " " arg) "*logalimacs*")
  )

(defun loga-search-word-in-hand-or-region ()
  "search word from logaling. if not mark region, search word type on manual. otherwise passed character inside region."
  (interactive)
  (let* ((word (loga-point-or-read-string)))
    (save-current-buffer
      (loga-logaling-prompt-command "lookup" word)
      )))

(defun loga-add-word ()
  "this is command to adding word, first origin word, second translated word."
  (interactive)
  (let* (origin translated)
    (setq origin (loga-point-or-read-string "here to adding word: ")
          translated (loga-point-or-read-string "here to translated word: " t)
          )
    (loga-logaling-prompt-command "add" (concat origin " " translated))
    )
  )

(defun loga-point-or-read-string (&optional prompt no-region)
  "If mark is active, return the region, otherwise, read string with PROMPT."
  (cond
   ((and mark-active (not no-region))
    (buffer-substring-no-properties (region-beginning) (region-end)))
   (t
    (read-string (or prompt "Search word here: ")))))

(provide 'logalimacs)
