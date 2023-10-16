;;; conf_general.el ---                              -*- lexical-binding: t; -*-

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

;; Backup files
(setq make-backup-files nil)


;; Startup
(setq inhibit-startup-screen t)
(setq initial-scratch-message "\
;; Don't Complain!

")

;; yes or no -> y or n
(fset 'yes-or-no-p 'y-or-n-p)

(global-auto-revert-mode t)

;; Tabs -> 4 spaces
(setq-default tab-width 4
              indent-tabs-mode nil)

(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Clean up white space
(add-hook 'before-save-hook 'whitespace-cleanup)


(recentf-mode 1)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)


;; File size
(setq gc-cons-threshold 50000000)
(setq large-file-warning-threshold 100000000)

;; Scroll settings
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)


(setq auto-insert t)

(provide 'conf_general)
;;; conf_general.el ends here
