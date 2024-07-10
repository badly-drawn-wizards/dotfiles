;;; org-utils.el --- Description -*- lexical-binding: t; -*-

(require 'org)
(require 'org-clock)

(require 'doom-lib)
(require 'config-org)

(defmacro with-widen (&rest body)
  `(save-excursion
     (save-restriction
       (widen)
       ,@body)))

(defmacro with-marker (marker &rest body)
  (let ((m (cl-gensym))
        (b (cl-gensym)))
    `(when-let
      ((,m ,marker)
       (,b (marker-buffer ,m)))
      (with-current-buffer ,b
        (with-widen
          (goto-char ,m)
          ,@body)))))

(defun my/org-clock-elapsed-clock-in-time ()
  (floor
   (org-time-convert-to-integer
    (if (and (org-clock-is-active) (= org-clock-marker (point-marker))) (org-time-since org-clock-start-time) 0))
   60))

(defun my/org-clock-get-clocked-time ()
  (+
   (org-clock-sum-current-item
             (org-clock-get-sum-start))
   (my/org-clock-elapsed-clock-in-time)))

(defun my/org-marker-heading (marker)
  (with-marker marker (org-no-properties (org-get-heading t t t t))))

(defun my/org-clock-info ()
  (json-serialize
   (let ((active (when (org-clock-is-active) 't)))
    (or
      (when-let
        ((task (if active org-clock-marker (car org-clock-history)))
         (task-buffer (marker-buffer task)))
        (ht
          ("active" (or active :false))
          ("heading"
           (or (my/org-marker-heading task) :null))
          ("clockedMinutes"
           (or (with-marker task (my/org-clock-elapsed-clock-in-time)) :null))
          ("totalMinutes"
           (or (with-marker task (my/org-clock-get-clocked-time)) :null))
          ("recent"
           (map 'array #'my/org-marker-heading org-clock-history))
          ("buffer"
           (buffer-file-name task-buffer))
          ("point"
           (marker-position task))))
      :null))))

(defun my/org-clock-info-clock-in (i)
  (with-marker (nth i org-clock-history) (org-clock-in nil nil)))

(defun my/org-batch-agenda ()
  (org-no-properties
        (org-batch-agenda
          "a"
          org-agenda-span (quote month))))

(provide 'org-utils)
;;; org-utils.el ends here
