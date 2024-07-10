;;; config-treesitter.el --- Description -*- lexical-binding: t; -*-

(use-package! tree-sitter
  :init
  (defadvice tsc-dyn-get--download (around tsc-dont-download (&rest arg)))
  :config
  (cl-pushnew (expand-file-name "~/.tree-sitter") tree-sitter-load-path)
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(provide 'config-treesitter)
;;; config-treesitter.el ends here
