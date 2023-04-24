;; say-region.el --- Send the region to `say' command -*- lexical-binding: t -*-

;; Author: Bernardo Barros
;; URL: https://github.com/smoge/say-region.el/
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package allows you to send the selected region in Emacs to the `say'
;; command.  To use it, simply select a region and press "C-c s" to start
;; the `say' process.  To kill the running `say' process, press "C-c k".

;;; Code:

(defgroup say-region nil
  "Send the region to the `say' command."
  :prefix "say-region-"
  :group 'external
  :link '(url-link :tag "GitHub" "https://github.com/smoge/say-region.el/")
  )

(defun say-region ()
  "Send the region to `say'."
  (interactive)
  (let ((proc (start-process "say-process" "say-buffer" "say" )))
    (process-send-region proc (region-beginning) (region-end))))

(defun say-region-kill-process ()
  "Kill running say processes in current buffer."
  (interactive)
  (let ((processes (process-list)))
    (dolist (process processes)
      (when (string-prefix-p "say" (process-name process))
        (delete-process process)))))

(global-set-key (kbd "C-c s") 'say-region)
(global-set-key (kbd "C-c k") 'say-region-kill-process)

(provide 'say-region)

;;; say-region.el ends here
