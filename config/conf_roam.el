;;; conf_roam.el ---                                 -*- lexical-binding: t; -*-

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

;; https://hugocisneros.com/org-config/#org-roam

;;; Code:


(setq org-roam-directory "~/org/roam")
(setq-default org-download-image-dir "~/org/roam/assets")
(setq org-download-screenshot-method "screencapture")

(with-eval-after-load 'org-roam
  ;; Roam is always one level deep in my org-directory
  (setq org-id-link-to-org-use-id t)
  (setq org-roam-completion-system 'helm)
  (add-to-list 'display-buffer-alist
               '(("\\*org-roam\\*"
                  (display-buffer-in-direction)
                  (direction . right)
                  (window-width . 0.33)
                  (window-height . fit-window-to-buffer))))
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :immediate-finish t
           :if-new (file+head "${slug}.org"
                              "#+TITLE: ${title}\n#+lastmod: Time-stamp: <>\n\n")
           :unnarrowed t)
          ("t" "temp" plain "%?"
           :if-new(file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+TITLE: ${title}\n#+lastmod: Time-stamp: <>\n\n")
           :immediate-finish t
           :unnarrowed t)
          ("p" "private" plain "%?"
           :if-new (file+head "${slug}-private.org"
                              "#+TITLE: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("a" "article" plain
           "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n%?\n* Notes\n\n"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+TITLE: ${title}\n#+FILETAGS: literature\n#+lastmod: Time-stamp: <>\n\n")
           :unnarrowed t)
          ("p" "project" plain
           "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
           :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+TITLE: ${title}\n#+FILETAGS: Project")
           :unnarrowed t)
          )
        )
  (org-roam-setup)
  (org-roam-db-autosync-mode)
  )
;;(setq org-id-extra-files (org-roam-list-files org-roam-directory))


;; Journal
;; (setq org-journal-dir "~/org/journal/")
;; (setq org-journal-enable-encryption nil)

(add-hook 'before-save-hook 'time-stamp) ;; Update time stamp with each save



(provide 'conf_roam)
;;; conf_roam.el ends here
