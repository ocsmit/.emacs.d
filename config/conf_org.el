;;; conf_org.el --- configuration for org-mode       -*- lexical-binding: t; -*-

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

(setq org-agenda-files '("~/org"))


;; Quickly open org files
(defun my-org-finder ()
  (interactive)
  (ido-find-file-in-dir "~/org/"))

(global-set-key (kbd "C-c f") 'my-org-finder)

;; record timestamp when TODO is set to done
(setq org-log-done 'time)

;; Follow links
(setq org-return-follows-link t)

;; Latex
(setq org-highlight-latex-and-related '(latex script))


;; Associate all files w/ org mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Make indentation nicer
(add-hook 'org-mode-hook 'org-indent-mode)

;; Remap change priority keys to use the up or down key
(define-key org-mode-map (kbd "C-c <up>") 'org-priority-up)
(define-key org-mode-map (kbd "C-c <down>") 'org-priority-down)

;; Shortcuts for stroing links, view agenda, and starting capture
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

;; Change level of org item, use SMR
(define-key org-mode-map (kbd "C-c C-g C-r") 'org-shiftmetaright)

;; Hide markers (e.g. *BOLD-TEXT* -> BOLD-TEXT)
(setq org-hide-emphasis-markers t)

;; Wrap lines
(add-hook 'org-mode-hook 'visual-line-mode)

(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  (unless (eq org-journal-file-type 'daily)
    (org-narrow-to-subtree))
  (goto-char (point-max)))


;; == Templates ================================================================
(setq org-capture-templates
      '(
        ;; Work log
        ("w" "Work Log Entry"
         entry (file+datetree "~/org/work-log.org")
         "* %?"
         :empty-lines 0)

        ;; Note dump
        ("n" "Note"
         entry (file+headline "~/org/notes.org" "Random Notes")
         "** %?"
         :empty-lines 0)

        ;; General todos
        ("g" "General To-Do"
         entry (file+headline "~/org/todos.org" "General Tasks")
         "* TODO [#B] %?\n:Created: %T\n "
         :empty-lines 0)

        ;; Links todo to line of code
        ("c" "Code To-Do"
         entry (file+headline "~/org/todos.org" "Code Related Tasks")
         "* TODO [#B] %?\n:Created: %T\n%i\n%a\nProposed Solution: "
         :empty-lines 0)

        ;; Meetings
        ;; - entry: sets up how structure of file will be saved
        ;; - next line describes template
        ("m" "Meeting"
         ;; sets up how structure of file will be saved
         entry (file+datetree "~/org/meetings.org")
         ;; describes template for how meeting will be recorded
         ;; - %T is special char for timestamp
         ;; - Populates Action Items as empty todo
         "* %? :meeting:%^g \n:Created: %T\n** Attendees\n*** \n** Notes\n** Action Items\n*** TODO [#A] "
         :tree-type week
         :clock-in t
         :clock-resume t
         :empty-lines 0)

        ("j" "Journal entry"
         plain (function org-journal-find-location)
         "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
         :jump-to-captured t :immediate-finish t)

        )
      )


;; == TODO opts ================================================================
;; States
(setq org-todo-keywords
      '((sequence "TODO(t)"
                  "PLANNING(p)"
                  "IN-PROGRESS(i@/!)"
                  "VERIFYING(v!)"
                  "BLOCKED(b@)"
                  "|"
                  "DONE(d!)"
                  "OBE(o@!)"
                  "WONT-DO(w@/!)"
                  )))
;; TODO colors
;; (setq org-todo-keyword-faces
;;       '(
;;         ("TODO" . (:foreground "GoldenRod" :weight bold))
;;         ("PLANNING" . (:foreground "DeepPink" :weight bold))
;;         ("IN-PROGRESS" . (:foreground "Cyan" :weight bold))
;;         ("VERIFYING" . (:foreground "DarkOrange" :weight bold))
;;         ("BLOCKED" . (:foreground "Red" :weight bold))
;;         ("DONE" . (:foreground "LimeGreen" :weight bold))
;;         ("OBE" . (:foreground "LimeGreen" :weight bold))
;;         ("WONT-DO" . (:foreground "LimeGreen" :weight bold))
;;         ))


;; == Tags =====================================================================
;; Tags
(setq org-tag-alist '(
                      ;; Ticket types
                      (:startgroup . nil)
                      ("@bug" . ?b)
                      ("@feature" . ?u)
                      ("@spike" . ?j)
                      (:endgroup . nil)

                      ;; Ticket flags
                      ("@write_future_ticket" . ?w)
                      ("@emergency" . ?e)
                      ("@research" . ?r)

                      ;; Meeting types
                      (:startgroup . nil)
                      ("sync" . ?i)
                      ("advisor" . ?a)
                      ("accenture" . ?g)
                      ("mutated" . ?s)
                      ("lab_meeting" . ?l)
                      (:endgroup . nil)

                      ;; Code TODOs tags
                      ("QA" . ?q)
                      ("backend" . ?k)
                      ("broken_code" . ?c)
                      ("frontend" . ?f)

                      ;; Special tags
                      ("CRITICAL" . ?x)
                      ("obstacle" . ?o)

                      ;; Meeting tags
                      ("general" . ?l)
                      ("meeting" . ?m)
                      ("misc" . ?z)
                      ("planning" . ?p)

                      ;; Work Log Tags
                      ("accomplishment" . ?a)
                      ))



;; Old
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
;;; Org mode agenda
;; (setq org-agenda-files (list "~/Documents/org/work.org"
;;                              "~/Documents/org/school.org"
;;                              "~/Documents/org/home.org"))
;; Org mode langs
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (R . t)
   (python . t)
   (julia . t)
   ))

;;; org modern
;;(with-eval-after-load 'org (global-org-modern-mode))

;;; src code security
(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("python" "R"))))

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis "…"

 ;; Agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "⭠ now ─────────────────────────────────────────────────")

(global-org-modern-mode)



(provide 'conf_org)
;;; conf_org.el ends here
