;; this program is frontend for myrurema
;; (requirement myrurema for Ruby gem and logalimacs)

;;; install (see also https://github.com/yhara/myrurema)
;; % gem install myrurema
;; % rurema --init
;;; when update database
;; % rurema --update

;; todo support multiple word
(defun loga-lookup-for-rurema (&optional word-for-fly-mode)
  ""
  (interactive)
  (let* ((word (or word-for-fly-mode
                   (loga-point-or-read-string "Search word here: "))))
    (save-current-buffer
      (loga-prompt-command "\\rurema" (concat word " --no-ask") nil t))))

(provide 'logalimacs-rurema)
