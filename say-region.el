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
  :link '(url-link :tag "GitHub" "https://github.com/smoge/say-region.el/"))

(defun say-region-command ()
  "Get the appropriate text-to-speech command based on the operating system."
  (cond ((eq system-type 'darwin)
         "say")
        ((eq system-type 'gnu/linux)
         "espeak-ng -d jack")
        (t (error "Unsupported operating system"))))

(defun say-region ()
  "Send the region to the appropriate text-to-speech command."
  (interactive)
  (if (use-region-p)
      (let* ((command-string (say-region-command))
             (command-parts (split-string command-string))
             (proc (apply #'start-process "say-process" "say-buffer" command-parts)))
        (process-send-region proc (region-beginning) (region-end)))
    (message "No active region to send to text-to-speech command.")))

(defun say-region-kill-process ()
  "Kill running say or espeak processes, depending on the operating system."
  (interactive)
  (let ((killed-count 0)
        (process-prefix (if (eq system-type 'gnu/linux) "espeak" "say")))
    (dolist (process (process-list))
      (when (string-prefix-p process-prefix (process-name process))
        (delete-process process)
        (setq killed-count (1+ killed-count))))
    (message (format "%d %s processes killed." killed-count process-prefix))))

(define-minor-mode say-region-mode
  "Toggle say-region mode."
  :lighter " SayR"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c s") 'say-region)
            (define-key map (kbd "C-c k") 'say-region-kill-process)
            map))

(provide 'say-region)

;;; say-region.el ends here
