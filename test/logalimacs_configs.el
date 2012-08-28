
(let* ((logalimacs-directory "~/bb/emacs.d/elisp/logalimacs/"))
  (dired logalimacs-directory)
  (load (concat logalimacs-directory "stem"))
  (load (concat logalimacs-directory "popup"))
  (load (concat logalimacs-directory "popwin"))
  (load (concat logalimacs-directory "logalimacs"))
  (setq loga-use-dictionary-option t
        loga-use-stemming t
        loga-use-singular-form t
        loga-use-auto-detect-language t)
  (global-set-key (kbd "M-g M-i") 'loga-interactive-command)
  (global-set-key (kbd "M-g M-u") 'loga-lookup-at-manually)
  (global-set-key (kbd "M-g M-a") 'loga-add)
  (global-set-key (kbd "C-:") 'loga-lookup-in-popup))
