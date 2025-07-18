;;; woman-signals.el --- man list to woman -*- lexical-binding: t -*-

;;; Commentary:
;;     Probably elementary stuff

;;; Code:

(require 'woman)
(require 'bookmark)

(let ((man '("_exit" "access" "alarm" "cfgetispeed" "cfgetospeed" "cfsetispeed" "cfsetospeed"
             "chdir" "chmod" "chown" "close" "creat" "dup" "dup2" "execle" "execve"
             "fcntl" "fork" "fpathconf" "fstat" "fsync" "getegid" "geteuid" "getgid"
             "getgroups" "getpgrp" "getpid" "getppid" "getuid" "kill" "link" "lseek"
             "mkdir" "mkfifo" "open" "pathconf" "pause" "pipe" "raise" "read" "rename"
             "rmdir" "setgid" "setpgid" "setsid" "setuid" "sigaction" "sigaddset"
             "sigdelset" "sigemptyset" "sigfillset" "sigismember" "signal" "sigpending"
             "sigprocmask" "sigsuspend" "sleep" "stat" "sysconf" "tcdrain" "tcflow"
             "tcflush" "tcgetattr" "tcgetpgrp" "tcsendbreak" "tcsetattr" "tcsetpgrp"
             "time" "times" "umask" "uname" "unlink" "utime" "wait" "waitpid" "write"
             "sigpause" "sigset")))
  (dolist (entry man)
    (ignore-errors
      (woman entry)
      (let* ((buf-name (format "*WoMan %s*" entry))
             (buf (get-buffer buf-name)))
        (when buf
          (with-current-buffer buf
            (bookmark-store
             entry
             `((handler . woman-bookmark-jump)
               (buffer-name . ,(buffer-name buf))
               (position . ,(point-min)))
             nil))))))
  (bookmark-save))

(provide 'woman-signals)
;;; woman-signals.el ends here
