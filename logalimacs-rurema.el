;; This program is frontend for myrurema
;; (requirement myrurema for Ruby gem and logalimacs)

;; Copyright (C) 2011, 2012 by Yuta Yamada

;; Author: Yuta Yamada <yamada@clear-code.com>
;; URL: https://github.com/logaling/logalimacs

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

;; install myrurema (see also https://github.com/yhara/myrurema)
;; % gem install myrurema
;; % rurema --init
;;; when update database
;; % rurema --update


;;;###autoload (autoload 'loga-lookup-for-rurema "logalimacs")


;; todo support multiple word
;;;###autoload
(defun loga-lookup-for-rurema (&optional word-for-fly-mode)
  ""
  (interactive)
  (let* ((word (or word-for-fly-mode
                   (loga-point-or-read-string "Search word here: "))))
    (save-current-buffer
      (loga-prompt-command "\\rurema" (concat word " --no-ask") nil t))))

(provide 'logalimacs-rurema)
