;; if you not like behavior of po-mode,
;; you can customize behavior by apply below code when create window.

;;; example
;; (defadvice po-subedit-create-buffer
;;   (around ad-po-subedit-create-buffer activate)
;;   "this is wraped subedit-create-buffer for user"
;;   (let* ((origin (current-buffer)))
;;     (progn
;;       (if (one-window-p)
;;           ad-do-it
;;         (windmove-left)
;;         (if (equal origin (current-buffer))
;;             (windmove-down)
;;           )
;;         (switch-to-buffer (ad-get-arg 0))
;;         )
;;       )
;;     ))

(when (require 'po-mode nil t)
  (defun po-subedit-abort ()
    "Exit the subedit buffer, merely discarding its contents."
    (interactive)
    (let* ((edit-buffer (current-buffer))
           (back-pointer po-subedit-back-pointer)
           (entry-marker (nth 0 back-pointer))
           (overlay-info (nth 2 back-pointer))
           (entry-buffer (marker-buffer entry-marker)))
      (if (null entry-buffer)
          (error (_"Corresponding PO buffer does not exist anymore"))
        ;;fix: not delete other-window
        ;(or (one-window-p) (delete-window))
        (or (one-window-p) (kill-this-buffer))
        ;;fix: not swicth current buffer
        ;(switch-to-buffer entry-buffer)
        (switch-to-buffer-other-window entry-buffer)
        (goto-char entry-marker)
        (and overlay-info (po-dehighlight overlay-info))
        (kill-buffer edit-buffer)
        (setq po-edited-fields (delete back-pointer po-edited-fields)))))

  (defun po-edit-string (string type expand-tabs)
    "Prepare a pop up buffer for editing STRING, which is of a given TYPE.
TYPE may be 'comment or 'msgstr.  If EXPAND-TABS, expand tabs to spaces.
Run functions on po-subedit-mode-hook."
    (let ((marker (make-marker)))
      (set-marker marker (cond ((eq type 'comment) po-start-of-msgid)
                               ((eq type 'msgstr) po-start-of-msgstr-form)))
      (if (po-check-for-pending-edit marker)
          (let ((edit-buffer (generate-new-buffer
                              (concat "*" (buffer-name) "*")))
                (edit-coding buffer-file-coding-system)
                (buffer (current-buffer))
                overlay slot)
            (if (and (eq type 'msgstr) po-highlighting)
                ;; ;; Try showing all of msgid in the upper window while editing.
                ;; (goto-char (1- po-start-of-msgstr-block))
                ;; (recenter -1)
                (save-excursion
                  (goto-char po-start-of-entry)
                  (re-search-forward po-any-msgid-regexp nil t)
                  (let ((end (1- (match-end 0))))
                    (goto-char (match-beginning 0))
                    (re-search-forward "msgid +" nil t)
                    (setq overlay (po-create-overlay))
                    (po-highlight overlay (point) end buffer))))
            (setq slot (list marker edit-buffer overlay)
                  po-edited-fields (cons slot po-edited-fields))
            ;;fix: define function for user access
            ;; (pop-to-buffer edit-buffer)
            (po-subedit-create-buffer edit-buffer)
            (text-mode)
            (set (make-local-variable 'po-subedit-back-pointer) slot)
            (set (make-local-variable 'indent-line-function)
                 'indent-relative)
            (setq buffer-file-coding-system edit-coding)
            (setq local-abbrev-table po-mode-abbrev-table)
            (erase-buffer)
            (insert string "<")
            (goto-char (point-min))
            (and expand-tabs (setq indent-tabs-mode nil))
            (use-local-map po-subedit-mode-map)
            (if (fboundp 'easy-menu-define)
                (progn
                  (easy-menu-define po-subedit-mode-menu po-subedit-mode-map ""
                    po-subedit-mode-menu-layout)
                  (and po-XEMACS (easy-menu-add po-subedit-mode-menu))))
            (set-syntax-table po-subedit-mode-syntax-table)
            (run-hooks 'po-subedit-mode-hook)
            (message po-subedit-message)))))

  ;;add function
  (defun po-subedit-create-buffer(buffer-name)
    "you can customize to this function, 
  example:
  \(defadvice po-subedit-create-buffer
    \(around ad-po-subedit-create-buffer activate\)
    \"this is wraped subedit-create-buffer for user\"
    \(progn
      \(if \(one-window-p\)
          ad-do-it
        \(windmove-left\)
        \(if \(equal origin \(current-buffer\)\)
              \(windmove-down\)
          \)
        \(switch-to-buffer \(ad-get-arg 0\)
        \)\)
      \)\)"
    (pop-to-buffer buffer-name)
    )
  )

