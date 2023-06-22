;;; nix-prefetch.el --- Description -*- lexical-binding: t; -*-

(defcustom nix-fetchers-list '("fetchurl" "fetchgit" "fetchFromGitHub")
  "List of nix fetchers for selecting fetcher at point"
  :type '(list string))

;;;###autoload
(defun nix-prefetch ()
  (interactive)
  (unless mark-active
    (unless (nix-a-fetcher) (user-error "Could not find fetcher to prefetch")))
  (if mark-active
      (progn
        (goto-char (1+ (point)))
        (nix-prefetch-region)
        (setq mark-active nil))
      (error "Impossible: mark not active should be unreachable here")))

;;;###autoload
(defun nix-a-fetcher (&optional dir)
  (interactive)
  (if dir
   (let ((re (eval `(rx (or ,@nix-fetchers-list))))
         (prev (point)))
     (condition-case _
       (progn
         (cl-case dir
           ('forward (search-forward-regexp re))
           ('backward (search-backward-regexp re)))
         (let ((start (match-beginning 0))
               (end
                 (progn
                   (search-forward "{")
                   (goto-char (1+ (point)))
                   (end-of-thing 'list)
                   (goto-char (1- (point)))
                   (point))))
           (set-mark start)
           (goto-char end)
           (setq mark-active 't)
           't))
       ((search-failed user-error)
        (goto-char prev)
        nil)))
   (let ((prev (point))
         (backwards (nix-a-fetcher 'backward)))
     (if
       (and
        backwards
        (<= (mark) prev)
        (<= prev (point)))
       't
       (progn
         (goto-char prev)
         (nix-a-fetcher 'forward))))))


;;;###autoload
(defun nix-prefetch-region (&optional start end)
  (interactive (list (region-beginning) (region-end)))
  (unless start (setq start (region-beginning)))
  (unless end (setq end (region-end)))
  (message "region %d %d" start end)
  (if-let* ((region (buffer-substring start end))
            (args (list "--output" "nix" "-s" "-E" region))
            (replacement
              (with-temp-buffer
                (let ((exit-code (apply 'call-process "nix-prefetch" nil (current-buffer) nil args)))
                  (if (= 0 exit-code)
                    (progn
                      (replace-regexp-in-string
                        "\n\\'" "" (buffer-string)))
                    (progn
                      (message "region was:\n%s" region)
                      (message "nix-prefetch-region failed with:\n%s" (buffer-string))
                      nil))))))
      (save-excursion
        (goto-char start)
        (if (re-search-forward "{[^}]+}" end 't)
            (progn
              (replace-match replacement)
              (indent-region start (point)))))))

(provide 'nix-prefetch)
;;; nix-prefetch.el ends here
