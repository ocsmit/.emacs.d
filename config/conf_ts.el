;;; conf_ts.el ---                                   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Owen Smith

;; Author: Owen Smith  <osmith2@ncsu.edu>
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


(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode))


(setq treesit-language-source-alist
      '(
        (python "https://github.com/tree-sitter/tree-sitter-python.git")
        (r "https://github.com/r-lib/tree-sitter-r.git")
        (bash "https://github.com/tree-sitter/tree-sitter-bash.git")
        (css "https://github.com/tree-sitter/tree-sitter-css.git")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp.git")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (julia "https://github.com/tree-sitter/tree-sitter-julia")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (c "https://github.com/tree-sitter/tree-sitter-c.git")
        )
      )
;;(add-hook 'ess-r-mode-hook 'tree-sitter-ess-r-mode-activate)



(provide 'conf_ts)
;;; conf_ts.el ends here
