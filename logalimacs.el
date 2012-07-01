;;; -*- coding: utf-8; lexical-binding: t -*-
;;; logalimacs.el --- Front-end to logaling-command for Ruby gems

;; Copyright (C) 2011, 2012 by Yuta Yamada

;; Author: Yuta Yamada <cokesboy"at"gmail.com>
;; URL: https://github.com/logaling/logalimacs
;; Version: 1.0.1
;; Package-Requires: ((popwin "20120529") (popup "20120331"))
;; Keywords: translation, logaling-command

;;; License:
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

(eval-when-compile
  (require 'cl))

(require 'popwin)
(require 'popup)

;; for word-at-point
(require 'thingatpt)

;; for spaces-string
(require 'rect)

;; json
(require 'json)

;; for ansi-color
(require 'ansi-color)

;; for flymake-err-info
(require 'flymake)

(defcustom loga-popup-output-type :auto
  "assign 'auto or 'max, available modifying of popup width"
  :group 'logalimacs
  :type 'symbol)

(defcustom loga-log-output nil
  "if nonnil, output log for developer."
  :group 'logalimacs
  :type 'boolean)

(defcustom loga-cascade-output t
  "if nonnil, output by cascade popup"
  :group 'logalimacs
  :type 'boolean)

(defcustom loga-fly-mode-interval 1
  "timer-valiable for loga-fly-mode, credit par sec."
  :group 'logalimacs
  :type 'integer)

(defvar loga-fly-timer nil
  "timer object for loga-fly-mode")

(defcustom loga-popup-margin 0
  "margin variable for popup-tip"
  :group 'logalimacs
  :type 'integer)

(defcustom loga-word-cache-limit 10
  "number of cached words"
  :group 'logalimacs
  :type 'integer)

(defcustom loga-width-limit-source 30
  "limit width of source word"
  :group 'logalimacs
  :type 'integer)

(defcustom loga-width-limit-target 0
  "limit of width of target word"
  :group 'logalimacs
  :type 'integer)

(defcustom loga-use-dictionary-option nil
  "If nonnil, use --dictionary for lookup option, It can use at more than logaling version 0.1.3"
  :group 'logalimacs
  :type 'boolean)

(defvar loga-fly-mode nil "if nonnil, logalimacs use loga-fly-mode")
(defvar loga-word-cache nil "cache word used by loga-lookup")
(defvar loga-current-command nil "get executed current command-name and symbol")
(defvar loga-current-endpoint nil "store current endpoint symbol")
(defvar loga-current-max-length nil)
(defvar loga-base-buffer nil)
(defvar loga-popup-point 0)
(defvar loga-popup-width 0)
(defvar loga-fallback-function nil "
Allow your favorite function.
It will be execute when the lookup was failed.
Example:
  (setq loga-fallback-function
      (lambda (search-word)
        (my/super-translation-function search-word)))")

(defvar loga-command-alist
  '((?a . :add)
    (?c . :config)
    (?d . :delete)
    (?h . :help)
    (?i . :import)
    (?l . :lookup)
    (?L . :list)
    (?n . :new)
    (?r . :register)
    (?U . :unregister)
    (?u . :update)
    (?s . :show)
    (?v . :version)))

(defvar loga-buffer-or-popup-command-alist
  '((?b . :buffer)
    (?q . :quit)
    (?n . :next-line)
    (?p . :previous-line)
    (?j . :next-line)
    (?k . :previous-line)
    (?d . :detail)))

(defvar loga-popup-menu-keymap
  (let ((map (copy-keymap popup-menu-keymap)))
    (define-key map "q" 'keyboard-quit)
    (define-key map "d" 'loga-lookup-in-buffer)
    (define-key map "n" 'popup-next)
    (define-key map "p" 'popup-previous)
    (define-key map "j" 'popup-next)
    (define-key map "k" 'popup-previous)
    (define-key map "f" 'popup-open)
    (define-key map "b" 'popup-close)
    map))

(defun loga-response-of-event (command-alist)
  (assoc-default last-input-event command-alist))

;;;###autoload
(defun loga-interactive-command ()
  "interactive-command for logaling-command, types following mini-buffer."
  (interactive)
  (read-event "types prefix of feature that want you :\n a)dd,c)onfig,d)elete,h)elp,i)mport,l)ookup,L)ist,n)ew,r)egister,U)nregister,u)pdate,v)ersion")
  (setq loga-current-command (loga-response-of-event loga-command-alist))
  (case loga-current-command
    (:lookup (loga-lookup-at-manually))
    (t       (loga-command))))

(defun loga-buffer-or-popup-command ()
  (read-event "")
  (let ((event (loga-response-of-event loga-buffer-or-popup-command-alist))
        (scroll-logalimacs-buffer
         (lambda (up-or-down)
           (unless (eq loga-current-endpoint :popup)
             (scroll-other-window up-or-down)
             (loga-buffer-or-popup-command)))))
    (case event
      (:next-line     (funcall scroll-logalimacs-buffer 1))
      (:previous-line (funcall scroll-logalimacs-buffer -1))
      (:buffer        (loga-make-buffer (cdar loga-word-cache)))
      (:quit          (loga-quit))
      (:detail        (loga-display-detail)))))

(defun loga-display-detail ()
  "If popup where current endpoint, output to buffer. if buffer, quit buffer"
  (case loga-current-endpoint
    (:buffer (loga-quit))
    (:popup  (loga-lookup-in-buffer))))

;; @todo apply ansi-color
(defun loga-to-shell (cmd &optional arg)
  (ansi-color-apply (shell-command-to-string (concat cmd " " arg " &"))))

(defun loga-from-symbol-to-string (symbol)
  (replace-regexp-in-string ":" "" (symbol-name symbol)))

(defun loga-command (&optional search-word)
  (let* ((loga "\\loga")
         (task (loga-from-symbol-to-string loga-current-command))
         (word-and-options (loga-lookup-attach-option search-word)))
    (setq loga-base-buffer (current-buffer))
    (case loga-current-command
      (:lookup             (loga-produce-contents search-word word-and-options))
      ((:add :update)      (loga-add/update task))
      ((:show :list)
       (loga-make-buffer   (loga-to-shell loga task)))
      ((:config :delete :help :import :new)
       (loga-make-buffer   (loga-to-shell loga (concat task " " (loga-input)))))
      ((:register :unregister :version)
       (minibuffer-message (loga-to-shell loga task))))))

(defun loga-produce-contents (search-word word-and-options)
  (let ((terminal-output
         (loga-to-shell "\\loga" (concat "lookup " word-and-options))))
    (loga-register-output (cons search-word terminal-output))
    terminal-output))

(defun loga-add/update (task)
  (let* ((input (loga-input))
         (spew-message (loga-to-shell "\\loga" (concat task " " input))))
    (if (and (string-match "^term '.+' already exists in '.+'" spew-message)
             (yes-or-no-p
              (format "%sAre you sure you want to 'update' followed by?"
                      spew-message)))
        (loga-update)
      (loga-quit))))

(defun loga-lookup-attach-option (search-word)
  (let* ((options '()))
    (if loga-use-dictionary-option
        (push "--dictionary" options))
    (if (eq loga-current-endpoint :popup)
        (push "--output=json" options))
    (concat search-word " " (mapconcat 'identity options " "))))

(defun loga-register-output (current-search-words)
  (let* ((cached-list-length (length loga-word-cache)))
    (cond ((<= loga-word-cache-limit cached-list-length)
           (setq loga-word-cache (loga-nthcar (- cached-list-length 1)
                                              loga-word-cache))))
    (push current-search-words loga-word-cache)))

(defun loga-nthcar (n list)
  (reverse (nthcdr (- (length list) n) (reverse list))))

;;;###autoload
(defun loga-add ()
  "this is command to adding word, first source word, second target word."
  (interactive)
  (setq loga-current-command :add)
  (loga-command))

;;;###autoload
(defun loga-update ()
  "update to registered word"
  (interactive)
  (setq loga-current-command :update)
  (loga-command))

(defun loga-lookup (endpoint)
  (let* ((loga-current-command :lookup)
         (loga-current-endpoint endpoint)
         (source-word (loga-decide-source-word))
         (terminal-output (loga-command (concat "\"" source-word "\""))))
    (if (string< "" terminal-output)
        (case endpoint
          (:popup  (loga-make-popup  terminal-output))
          (:buffer (loga-make-buffer terminal-output)))
      (if (functionp loga-fallback-function)
          (loga-fallback source-word)
        (minibuffer-message
         (format "%s is not found" source-word))))))

(defun loga-decide-source-word ()
  (if mark-active
      (buffer-substring-no-properties (region-beginning) (region-end))
    (if current-prefix-arg
        (loga-input)
      (loga-return-word-on-cursor))))

(defun loga-attach-lang-option-for-ja/en (word)
  (cond
   ((string-match "[ぁ-んァ-ン上-黑]" word)
    (return (concat word " -S=ja -T=en")))
   ((string-match "[a-zA-Z]" word)
    (return (concat word " -S=en -T=ja")))))

(defun loga-convert-from-json (raw-json-data)
  (let* ((mixed-list (json-read-from-string raw-json-data))
         (keywords (loga-extract-keywords-from mixed-list))
         (converted-list (loga-format keywords)))
    (if loga-cascade-output
        converted-list
      (loga-format-to-string converted-list))))

(defun loga-extract-keywords-from (all-data)
  (loop with keywords
        for translation-group across all-data
        collect (loga-trim-and-compute-length translation-group) into keywords
        finally return keywords))

(defun loga-trim-and-compute-length (translation-group)
  (loop with source and target and note
        with source-length and target-length
        for (key . statement) in translation-group do
        (case key
          ('source (setq source statement
                         source-length (loga-compute-length source)))
          ('target (setq target (loga-chop-target statement)
                         target-length (loga-compute-length target)))
          ('note   (setq note statement)))
        finally return `(,source ,target ,note ,source-length ,target-length)))

(defun loga-format-to-string (converted-list)
  `(mapconcat 'identity ,@converted-list "\n"))

(defun loga-format (words)
  (setq loga-current-max-length (loga-compute-max-length words))
  (loop with formated-words = '()
        with size = loga-current-max-length
        for (source target note source-length target-length) in words
        if (and (loga-less-than-window-half-p source-length)
                (> loga-width-limit-source source-length))
        collect (loga-append-margin source target note size) into formated-words
        finally return formated-words))

(defun loga-chop-target (raw-target)
  (let ((tmp-target-length (loga-compute-length raw-target))
        (window-half (/ (window-width) 2)))
    (if (< window-half tmp-target-length)
        (nth 1 (popup-fill-string raw-target window-half))
      raw-target)))

(defun loga-compute-max-length (words)
  (loop with max-source-length = 0
        with max-target-length = 0
        for (source target note source-length target-length) in words
        if (loga-clear-condition-p max-source-length max-target-length
                                   source-length target-length)
        do (setq max-source-length (max max-source-length source-length)
                 max-target-length (max max-target-length target-length))
        finally return (cons max-source-length max-target-length)))

(defun loga-clear-condition-p (max-source-length max-target-length
                               source-length target-length)
  (let ((more-than-max-p (or (< max-source-length source-length)
                             (< max-target-length target-length)))
        (less-than-window-half-p
         (loga-less-than-window-half-p source-length))
        (below-limit-p (< source-length loga-width-limit-source)))
    (and more-than-max-p less-than-window-half-p below-limit-p)))

(defun loga-less-than-window-half-p (source-length)
  (let* ((half (- (/ (window-width) 2) 2)))
    (< source-length half)))

(defun loga-compute-length (sentence)
  (loop with sum = 0
        with tokens = (string-to-list (split-string sentence ""))
        for token in tokens
        if (and (string< "" token)
                (multibyte-string-p token)
                (loga-ignore-character-p token))
        do      (setq sum (+ sum 2))
        else do (setq sum (+ sum 1))
        finally return sum))

(defun loga-ignore-character-p (token)
  "If mixed Japanese language, wrong count at specific character. because it escape character"
  (not (string-match "[\\ -/:->{-~\\?^]\\|\\[\\|\\]" token)))

(defun loga-append-margin (source target note max-length)
  (let* ((margin (- (car max-length) (loga-compute-length source)))
         (column (concat source (spaces-string margin) ":" target)))
    (if note
        `(,column ,(concat "\n" note))
      `(,column))))

(defun loga-query (&optional message)
  (let* ((input (read-string (or message "types here:"))))
    (case loga-current-command
      ((:add :update) (concat "\"" input "\""))
      (t input))))

(defun loga-input ()
  (let* ((query (loga-from-symbol-to-string loga-current-command))
         (task loga-current-command)
         (messages (concat query ": "))
         (loga-base-buffer (current-buffer)))
    (case task
      ((:add :update :config :delete :help :import :new
             :register :unregister)
       (loga-make-buffer (loga-to-shell "\\loga help" query))))
    (case task
      (:add    (setq messages '("source: " "target: " "note(optional): ")))
      (:update (setq messages '("source: " "target(old): "
                                "target(new): " "note(optional): ")))
      (:lookup (setq messages '("search: ")))
      (t       (setq messages `(,messages))))
    (loop with response
          for message in messages
          collect (loga-query message) into response
          finally return (mapconcat 'identity response " "))))

;;;###autoload
(defun loga-lookup-at-manually ()
  "Search word from logaling. if not mark region, search word type on manual. otherwise passed character inside region."
  (interactive)
  (setq current-prefix-arg 4)
  (loga-lookup :buffer))

;;;###autoload
(defun loga-lookup-in-popup ()
  "Display the output of loga-lookup at tooltip, note require popup.el"
  (interactive)
  (loga-lookup :popup))

;;;###autoload
(defun loga-lookup-in-buffer ()
  (interactive)
  (loga-lookup :buffer))

(defun loga-return-word-on-cursor ()
  "return word where point on cursor"
  (let* (match-word)
    (save-excursion
      (setq match-word
            (if (looking-at "\\w")
                (word-at-point)
              (backward-word)
              (word-at-point)))
      (if loga-log-output (print match-word)) ;;log
      (if (string-match "[上-黑]" match-word)
          (loga-reject-hiragana match-word)
        match-word))))

(defun loga-reject-hiragana (string)
  (replace-regexp-in-string "[ぁ-ん]" "" string))

(defun loga-make-buffer(content)
  (setq loga-current-endpoint :buffer
        other-window-scroll-buffer "*logalimacs*")
  (with-temp-buffer
    (switch-to-buffer (get-buffer-create "*logalimacs*"))
    (setq buffer-read-only nil)
    (erase-buffer) ;;initialize
    (insert content)
    (goto-char 0)
    (setq buffer-read-only t))
  (switch-to-buffer loga-base-buffer)
  (popwin:popup-buffer
   (get-buffer-create "*logalimacs*")
   :noselect t :stick t :height 10 :position :top)
  (case loga-current-command
    ((:lookup :show :list)
     (loga-buffer-or-popup-command))))

(defun loga-make-popup (content)
  (let* ((converted-content (loga-convert-from-json content)))
    (setq loga-current-endpoint :popup)
    (loga-setup-point-and-width)
    (typecase converted-content
      (list
       (popup-cascade-menu converted-content
                           :point loga-popup-point
                           :width loga-popup-width
                           :keymap loga-popup-menu-keymap))
      (string
       (popup-tip converted-content
                  :margin loga-popup-margin
                  :point loga-popup-point
                  :width loga-popup-width)))))

(defun loga-compute-point ()
  (let* ((half (/ (window-width) 2))
         (quarter (/ half 2))
         (cursor (- (point) (point-at-bol))))
    (cond
     ((< half cursor)
      (+ (point-at-bol) quarter))
     (t (point)))))

(defun loga-popup-output-type ()
  (let ((type (symbol-name loga-popup-output-type)))
    (if (string-match ":" type)
        loga-popup-output-type
      (make-symbol (concat ":" type)))))

(defun loga-setup-point-and-width ()
  (case (loga-popup-output-type)
    (:auto (setq loga-popup-width (loga-compute-width)
                 loga-popup-point (loga-compute-point)))
    (:max  (setq loga-popup-width (window-width)
                 loga-popup-point (point-at-bol)))))

(defun loga-compute-width ()
  (loop for (source-length . target-length) in `(,loga-current-max-length)
        with sum = 0
        collect (+ source-length  target-length) into sum
        finally return (min (+ (car sum) 1) (window-width))))

;;;###autoload
(defun loga-fly-mode ()
  "toggle loga-fly-mode-on and loga-fly-mode-off"
  (interactive)
  (if loga-fly-mode
      (loga-fly-mode-off)
    (loga-fly-mode-on)))

(defun loga-fly-mode-on ()
  (setq loga-fly-mode t
        loga-fly-timer
        (run-with-idle-timer loga-fly-mode-interval t
                             (lambda()
                               (loga-lookup-in-buffer))))
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
        (let* ((file      (flymake-ler-file (nth (1- count)
                                                 line-err-info-list)))
               (full-file (flymake-ler-full-file (nth (1- count)
                                                      line-err-info-list)))
               (text      (flymake-ler-text (nth (1- count)
                                                 line-err-info-list)))
               (line      (flymake-ler-line (nth (1- count)
                                                 line-err-info-list))))
          (loga-make-buffer (format "[%s] %s" line text))))
      (setq count (1- count)))))

(defun loga-quit ()
  (switch-to-buffer "*logalimacs*")
  (when (eq loga-current-endpoint :buffer)
    (quit-window)
    (switch-to-buffer loga-base-buffer))
  (keyboard-quit))

(defun loga-check-state ()
  (interactive)
  (let* ((ruby '(lambda (arg)
                  (shell-command-to-string (concat "ruby -e " arg))))
         (version (funcall ruby "'print RUBY_VERSION'"))
         (installed-p
          (not (string-match "no such file to load"
                             (funcall ruby "'require \"logaling\"'"))))
         (rvm-p (eq 0 (shell-command "which rvm"))))
    (cond
     ((and installed-p version)
      (message "Check OK: logaling-command already installed")
      t)
     ((not (string-match "1.9.[0-9]\\|[2-9].[0-9].[0-9]" version))
      (message "Note: Ruby version errer, require Ruby 1.9.x"))
     (rvm-p
      (if (require 'rvm nil t)
          (message "Note: require 'gem install logaling-command'")
        (message "Note: if use rvm, require rvm.el and sets the config to your dot emacs.")))
     (t (message "Note: require 'sudo gem install logaling-command'")))))

(defun loga-version-number ()
  (let* ((version-string (loga-to-shell "\\loga version")))
    (string-match "[0-9].[0-9].[0-9]" version-string)
    (match-string 0 version-string)))

(defun loga-fallback (search-word)
  (funcall loga-fallback-function search-word))

(provide 'logalimacs)

;;; logalimacs.el ends here
