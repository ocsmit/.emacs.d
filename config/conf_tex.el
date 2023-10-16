;;; conf_tex.el ---                                  -*- lexical-binding: t; -*-

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

;; https://kevinlanguasco.wordpress.com/2019/04/29/latex-editing-with-emacs-on-manjaro/

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)


(provide 'conf_tex)
;;; conf_tex.el ends here
