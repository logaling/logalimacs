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

(eval-when-compile
  (require 'cl))

;;for word-at-point
(require 'thingatpt)

;;json
(require 'json)

;;for ansi-color
(require 'ansi-color)

(defvar loga-fly-mode nil)
(defvar loga-log-output nil "if nonnil, output log for developer.")
(defvar loga-fly-mode-interval 1
  "timer-valiable for loga-fly-mode, credit par sec.")
(defvar loga-fly-timer nil)
(defvar loga-popup-margin 0)
(defvar loga-word-cache-limit 10)
(defvar loga-word-cache nil "cache word used by loga-lookup")
(defvar loga-current-command nil "get executed current command-name and symbol")
(defvar loga-current-endpoint nil "store current endpoint symbol")
(defvar loga-base-buffer nil)
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
    (?v . :version)
    ;(?f . :loga-fly-mode)
    ))

(defvar loga-buffer-or-popup-command-alist
  '((?b . :buffer)
    (?q . :quit)
    (?n . :next-line)
    (?p . :previous-line)
    (?d . :detail)))

(defun loga-check-state ()
  (interactive)
  (let* ((ruby '(lambda (arg)
                  (shell-command-to-string (concat "ruby -e " arg))))
         (version (funcall ruby "'print RUBY_VERSION'"))
         (installed-p
          (not (string-match "no such file to load"
                             (funcall ruby "'require \"logaling\"'"))))
         (rvm-p (eq 0 (shell-command "which rvm"))))
    (if (string-match
         "version [0-9].[1-9].[3-9]" (loga-to-shell "\\loga version"))
        (defvar loga-possible-json-p t)
      (defvar loga-possible-json-p nil))
    (cond
     ((or installed-p version)
      (print "logaling-command is already installed, check OK!") t)
     ((not (string-match "1.9.[0-9]\\|[2-9].[0-9].[0-9]" version))
      (print "Ruby version errer, require Ruby 1.9.x"))
     (rvm-p
      (if (require 'rvm nil t)
          (print "require 'gem install logaling-command'")
        (print
         "if use rvm, require rvm.el and sets the config to your dot emacs.")))
     (print "require 'sudo gem install logaling-command'"))))

;;;###autoload
(defun loga-interactive-command ()
  "interactive-command for logaling-command, types following mini-buffer."
  (interactive)
  (let* (task)
    (read-event "types prefix of feature that want you :\n a)dd,c)onfig,d)elete,h)elp,i)mport,l)ookup,n)ew,r)egister,U)nregister,u)pdate,v)ersion")
    (setq task (assoc-default last-input-event loga-command-alist))
    (loga-current-command task)
    (case task
      (:add (loga-add))
      (:lookup (loga-lookup-region-or-manually))
      (:update (loga-update))
      (t (loga-command)))))

(defun loga-buffer-or-popup-command ()
  (case (car loga-current-command)
    (:lookup
     (read-event)
     (case (assoc-default last-input-event loga-buffer-or-popup-command-alist)
       (:next-line (scroll-other-window 1) (loga-buffer-or-popup-command))
       (:previous-line (scroll-other-window-down 1)(loga-buffer-or-popup-command))
       (:buffer (loga-make-buffer (cdar loga-word-cache)))
       (:quit (if (eq loga-current-endpoint :popup)
                  (kill-buffer "*logalimacs*"))
              (keyboard-quit))
       (:detail (loga-display-detail))))))

(defun loga-display-detail ()
  "If popup where endpoint, output to buffer. if buffer, quit buffer"
  (case loga-current-endpoint
    (:buffer
     (kill-buffer "*logalimacs*"))
    (:popup
     (loga-make-buffer (cdar loga-word-cache)))))

;; @todo apply ansi-color
(defun loga-to-shell (cmd &optional arg help)
  (ansi-color-apply (shell-command-to-string (concat cmd " " arg " &"))))

(defun loga-current-command (symbol)
  (setq loga-current-command
        (cons symbol (loga-from-symbol-to-string symbol))))

(defun loga-from-symbol-to-string (symbol)
  (replace-regexp-in-string ":" "" (symbol-name symbol)))

(defun loga-command (&optional arg)
  (let* ((cmd "\\loga")
         (task (cdr loga-current-command))
         (symbol (car loga-current-command))
         (word (loga-lookup-attach-option arg)))
    (setq loga-base-buffer (current-buffer))
    (case symbol
      (:lookup
        (loga-word-cache (cons arg (loga-to-shell cmd (concat task " " word))))
        (cdar loga-word-cache))
      ((or :add :update)
       (loga-to-shell cmd (concat task " " arg)))
      (:show
       (loga-make-buffer (loga-to-shell cmd task)))
      ((or :config :delete :help :import :new :show)
       (loga-make-buffer (loga-to-shell cmd (concat task " " (loga-input)))))
      ((or :list :register :unregister :version)
       (minibuffer-message (loga-to-shell cmd task))))))

(defun loga-lookup-attach-option (arg)
  (let* ((word arg))
    (if (and loga-possible-json-p (eq loga-current-endpoint :popup))
        (setq word (concat arg " --output=json")))
    word))

(defun loga-convert-from-json-to-list (content)
  (let* ((json (json-read-from-string content))
         source target note words-list)
    (loop for record across json do
          (loop for pair in record do
                (case (car pair)
                  ('source (setq source (cdr pair)))
                  ('target (setq target (cdr pair)))
                  ('note   (setq note   (cdr pair)))))
          (push (cons (concat source " " target) note) words-list))
    (reverse words-list)))

(defun loga-word-cache (word)
  (cond ((<= loga-word-cache-limit (length loga-word-cache))
         (setq loga-word-cache (reverse loga-word-cache)
               loga-word-cache (cdr loga-word-cache)
               loga-word-cache (reverse loga-word-cache))))
  (push word loga-word-cache))

;;;###autoload
(defun loga-add ()
  "this is command to adding word, first source word, second target word."
  (interactive)
  (loga-current-command :add)
  (loga-command (loga-input)))

;;;###autoload
(defun loga-update ()
  "update to registered word"
  (interactive)
  (loga-current-command :update)
  (loga-command (loga-input)))

(defun loga-lookup (&optional endpoint manual?)
  (let* (word content)
    (loga-current-command :lookup)
    (setq word
          (if mark-active
              (buffer-substring-no-properties (region-beginning) (region-end))
            (case manual?
              (:manual (loga-input))
              (t (loga-return-word-on-cursor)))))
    (setq content (loga-command word))
    (case endpoint
      (:popup
        (if loga-possible-json-p
            (setq content (loga-convert-from-json-to-list content)))
        (loga-make-popup content))
      (t (loga-make-buffer content)))))

(defun loga-query (&optional message)
  (let* ((input (read-string (or message "types here:"))))
    (case (car loga-current-command)
      ((or :add :update :lookup) (concat "\"" input "\""))
      (t input))))

(defun loga-input ()
  (let* ((query (cdr loga-current-command))
         (task (car loga-current-command))
         (messages (concat query ": "))
         store)
    (case task
      ((or :add :update :config :delete :help :import :new
           :list :register :unregister)
       (loga-make-buffer (loga-to-shell "\\loga help" query))))
    (case task
      (:add (setq messages '("source: " "target: " "note(optional): ")))
      (:update (setq messages '("source: " "target(old): " "target(new): " "note(optional): ")))
      (:lookup (setq messages '("search: ")))
      (t (setq messages (list messages))))
    (loop for msg in messages do
          (push (loga-query msg) store))
    (mapconcat 'identity (reverse store) " ")))

;;;###autoload
(defun loga-lookup-at-manually ()
  "search word from logaling. if not mark region, search word type on manual. otherwise passed character inside region."
  (interactive)
  (setq loga-current-endpoint :buffer)
  (loga-lookup nil :manual))

;;;###autoload
(defun loga-lookup-in-popup ()
  "Display the output of loga-lookup at tooltip, note require popup.el"
  (interactive)
  (setq loga-current-endpoint :popup)
  (if current-prefix-arg
      (loga-lookup :popup :manual)
    (loga-lookup :popup nil))
  (loga-buffer-or-popup-command))

;;;###autoload
(defun loga-lookup-in-buffer ()
  (interactive)
  (if current-prefix-arg
      (loga-lookup nil :manual)
    (loga-lookup nil nil))
  (loga-buffer-or-popup-command))

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

(defun loga-reject-hiragana (str)
  (replace-regexp-in-string "[ぁ-ん]" "" str))

(defun loga-make-buffer(content)
  "create buffer for logalimacs"
  (setq loga-current-endpoint :buffer)
  (setq other-window-scroll-buffer "*logalimacs*")
  (with-temp-buffer
    (switch-to-buffer (get-buffer-create "*logalimacs*"))
    (erase-buffer) ;;initialize
    (insert content)
    (beginning-of-buffer))
  (switch-to-buffer loga-base-buffer)
  (popwin:popup-buffer
   (get-buffer-create "*logalimacs*")
   :noselect t :stick t :height 10 :position :top)
  (loga-buffer-or-popup-command))

(defun loga-make-popup (content)
  (setq loga-current-endpoint :popup)
  (cond
   ((not (require 'popup nil t))
    (message "Can't lookup, it is require popup.el."))
   ((equal "" content)
    (message (concat "'" (caar loga-word-cache) "' is not found")))
   ((listp content) (popup-menu content))
   (t (popup-tip content :margin loga-popup-margin))))

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

(loga-check-state)

(provide 'logalimacs)

;;; logalimacs.el ends here
