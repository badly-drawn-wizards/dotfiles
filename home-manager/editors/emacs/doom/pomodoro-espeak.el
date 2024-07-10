;;; pomodoro-espeak.el --- Description -*- lexical-binding: t; -*-

(defvar pomodoro-espeak--compliments
  '("you sexy beast"
    "you coding machine"
    "you beautiful mind"
    "now have some fun"
    "have a break"))

(defun pomodoro-espeak/speak (msg)
  (start-process "pomodoro-espeak" nil "espeak" msg))

(defun pomodoro-espeak/pomodoro-over ()
  (let*
      ((compliment (seq-random-elt pomodoro-espeak--compliments))
       (msg (format "Pomodoro is over %s" compliment)))
    (pomodoro-espeak/speak msg)))

(defun pomodoro-espeak/pomodoro-break-over ()
  (pomodoro-espeak/speak "Break is over. Back to work."))

(after! org-pomodoro
  (setq org-pomodoro-play-sounds nil)
  (add-hook 'org-pomodoro-finished-hook #'pomodoro-espeak/pomodoro-over)
  (add-hook 'org-pomodoro-break-finished-hook #'pomodoro-espeak/pomodoro-break-over))

(provide 'pomodoro-espeak)
;;; pomodoro-espeak.el ends here
