;;; conf_tabs.el ---                                 -*- lexical-binding: t; -*-

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

;; Ensure the needed tabs are created or exist; add extra to your liking
(progn (tab-create "eshells")
                                        ;(tab-create "exwm"))
       )

(defun tab-create (name)
  "Create the NAME tab if it doesn't exist already."
  (condition-case nil
      (unless (equal (alist-get 'name (tab-bar--current-tab))
                     name)
        (tab-bar-rename-tab-by-name name name))
    (error (tab-new)
           (tab-bar-rename-tab name))))


(provide 'conf_tabs)
;;; conf_tabs.el ends here
