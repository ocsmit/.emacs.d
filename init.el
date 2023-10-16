(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure 't)


;; Info
(setq user-mail-address "osmith2@ncsu.edu")

;; Custom paths
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/config")

(load "conf_packages")                  ; Package managment/install
(load "conf_vim")                       ; Set up evil mode
(load "conf_modeline")                  ; Mode line config
(load "conf_general")                   ; General config settings
(load "conf_projectile")                ; Project managment
(load "conf_ts")                        ; Treesitter config
(load "conf_languages")                 ; General language configs
(load "conf_org")                       ; Org-mode config
(load "conf_roam")                      ; Org-roam config
(load "conf_appearence")                ; Appearence config

(setq native-comp-async-report-warnings-errors nil)

;; Space as leader
(general-create-definer space-leader-def
  :prefix "SPC")

(space-leader-def
  :states '(normal visual)
  :keymaps 'override
  "w" (general-simulate-key "C-w" :state 'normal)
  "x" (general-simulate-key "C-x" :state 'normal)
  "c" (general-simulate-key "C-c" :state 'normal)
  "f" 'find-file
  "b"   '(:ignore t :which-key "buffer")
  "SPC"   (general-simulate-key "M-x")
  "p"   'projectile-command-map
  )

;; Map ctrl-g to esc
(global-set-key (kbd "<escape>")      'keyboard-escape-quit)

(defun reload-emacs-file ()
  "reload .emacs"
  (interactive)
  (load-file "~/.emacs.d/init.el")
  )

(set-face-attribute 'line-number nil :background nil)
(set-face-attribute 'fringe nil :background nil)
