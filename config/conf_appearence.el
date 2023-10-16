;;; conf_appearence.el --- appearence config         -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Owen Smith

;; Author: Owen Smith <osmith2@ncsu.edu>
;; Keywords:

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


(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;(global-tab-line-mode)
(windmove-default-keybindings)

;;; style
(setq tab-bar-show 1)                      ;; hide bar if <= 1 tabs open
(setq tab-bar-close-button-show nil)       ;; hide tab close / X button
(setq tab-bar-tab-hints t)                 ;; show tab numbers
(setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
(setq  tab-bar-auto-width-max nil)

;;; keybindings
;; ctrl-tab switches
;;(global-set-key (kbd "s-{") 'tab-bar-switch-to-prev-tab)
;;(global-set-key (kbd "s-}") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "s-t") 'tab-bar-new-tab)
(global-set-key (kbd "s-w") 'tab-bar-close-tab)

(global-set-key [remap list-buffers] 'ibuffer)

;; Start Fullscreen (cross-platf)
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)

;; Display line numbers
;;(global-display-line-numbers-mode 1)

;; only show line numbers in prog buffers
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

;; ruler
;;(global-display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
;;(setq display-fill-column-indicator-character '|')
(setq display-fill-column-indicator-column 80)
;;(setq display-fill-column-indicator-character '|')



;; Font
(set-frame-font "JetBrains Mono Light 13" nil t)
(setq text-scale-mode-step 1.05)

;; https://christiantietze.de/posts/2023/01/use-sf-pro-for-sf-symbols-everywhere-in-emacs/
(set-fontset-font t '(?􀀀 . ?􏿽) "SF Pro Display")

;; Tab bar format
(defun my/tab-bar-tab-name-format (tab i)
  (propertize
   (format " %d • %s " i (alist-get 'name tab))
   'face (funcall tab-bar-tab-face-function tab)))

(defun my/tab-bar-tab-name-function ()
  (or (projectile-project-name) (buffer-name)))

(setq tab-bar-tab-name-format-function #'my/tab-bar-tab-name-format)
(setq tab-bar-tab-name-function #'my/tab-bar-tab-name-function)


;; Theme
(load-theme 'wombat)
(set-face-background 'default "#111")
(set-face-foreground 'font-lock-comment-face "#fc0")

(set-face-attribute 'fringe nil :background nil)
(set-face-attribute 'line-number nil :background nil)
(set-face-attribute 'mode-line nil :background nil :box nil)
(set-face-attribute 'mode-line-inactive nil :background nil :box nil)

(provide 'conf_appearence)
;;; conf_appearence.el ends here
