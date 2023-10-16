;;; conf_languages.el ---                            -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Owen Smith

;; Author: Owen Smith <osmith2@ncsu.edu>
;; Keywords: lisp

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

;;

;;; Code:


;; Language specific indents
(add-hook 'sh-mode-hook
      (lambda ()
        (setq sh-basic-offset 2
          sh-indentation 2)))


(add-hook 'emacs-lisp-mode-hook
      (lambda ()
        (setq sh-basic-offset 2
          sh-indentation 2)))


(add-hook 'make-mode-hook
      (lambda ()
        (setq sh-basic-offset 2
          sh-indentation 2)))

;; ESS -------------------------------------------------------------------------
;; R, etc

(add-hook 'ess-mode-hook
          (lambda ()
            (ess-set-style 'Rstudio)
            (add-hook 'local-write-file-hooks
                       (lambda ()
                         (ess-nuke-trailing-whitespace)))))

(set 'ess-arg-function-offset t)

;; Python ----------------------------------------------------------------------
(setq elpy-shell-echo-output nil)
(setq python-shell-completion-native-enable nil)
;; (setq python-shell-interpreter "ipython"
;;       python-shell-interpreter-args "-i")

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-c exec('__import__(\\'readline\\')') -i --simple-prompt")


;; Julia -----------------------------------------------------------------------
(setq lsp-julia-default-environment "~/.julia/environments/v1.9")

;; Set the terminal backend
(setq julia-repl-set-terminal-backend 'vterm)

(defun my/julia-repl-send-cell()
  ;; "Send the current julia cell (delimited by ###) to the julia shell"
  (interactive)
  (save-excursion (setq cell-begin (if (re-search-backward "^###" nil t) (point) (point-min))))
  (save-excursion (setq cell-end (if (re-search-forward "^###" nil t) (point) (point-max))))
  (set-mark cell-begin)
  (goto-char cell-end)
  (julia-repl-send-region-or-line)
  (next-line))

;; Keybindings for quickly sending code to the REPL
;; (define-key julia-repl-mode-map (kbd "<C-RET>") 'my/julia-repl-send-cell)
;; (define-key julia-repl-mode-map (kbd "<M-RET>") 'julia-repl-send-line)
;; (define-key julia-repl-mode-map (kbd "<S-return>") 'julia-repl-send-buffer)

(provide 'conf_languages)
;;; conf_languages.el ends here
