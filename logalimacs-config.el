;;;* configuration for logalimacs

(require 'logalimacs nil t)
(require 'logalimacs-rurema nil t)

;;key-binds
;; @todo create loga-mode-map
(global-set-key (kbd "M-g M-u") 'loga-lookup-in-hand-or-region)
(global-set-key (kbd "M-g M-a") 'loga-add-word)
(global-set-key (kbd "M-g M-i") 'loga-interactive-command)

;;;*configuration for PopWin(requirement popwin.el)
(when (require 'popwin nil t)
  (defvar display-buffer-function 'popwin:display-buffer)
  (defvar popwin:special-display-config
    (append '(("*logalimacs*" :position top :height 10 :noselect t :stick t)))
    popwin:special-display-config))

(provide 'logalimacs-config)
