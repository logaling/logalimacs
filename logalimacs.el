;;;*this is wrapped program for logaling-command

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
