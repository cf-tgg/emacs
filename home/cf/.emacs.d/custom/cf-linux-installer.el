;;; cf/linux-installer.el --- Interactive LUKS+Linux blkencrypt -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(eval-when-compile
  (require 'subr-x)
  (require 'cl-lib))

(defgroup cf/linux-installer nil
  "Minimal Linux installer with LUKS support."
  :group 'external)

(defcustom cf/linux-supported-distros '("Artix" "Arch")
  "List of supported Linux distributions."
  :type '(repeat string))

(defcustom cf/linux-init-systems '(("runit" . "runit") ("systemd" . "systemd"))
  "Alist mapping init system names to package suffixes."
  :type '(alist :key-type string :value-type string))

(defcustom cf/linux-filesystems '("ext4" "btrfs" "xfs")
  "Supported filesystems for root partition."
  :type '(repeat string))

(defun cf/linux--run (cmd)
  (message "Running: %s" cmd)
  (unless (zerop (shell-command cmd))
    (error "Command failed: %s" cmd)))

(defun cf/linux--ask (prompt choices)
  (completing-read prompt choices nil t))

;;;###autoload
(defun cf/format-and-install-linux (&optional interactive)
  "Format device, encrypt with LUKS, and install Linux interactively."
  (interactive "p")
  (let* ((device (if interactive
                     (read-file-name "Block device (e.g. /dev/sdX): " "/dev/" nil t)
                   "/dev/sdX"))
         (distro (cf/linux--ask "Distro: " cf/linux-supported-distros))
         (init (cf/linux--ask "Init system: " (mapcar #'car cf/linux-init-systems)))
         (fs (cf/linux--ask "Filesystem: " cf/linux-filesystems))
         (luks-name "cryptroot"))

    ;; Partition
    (cf/linux--run (format "sgdisk --zap-all %s && echo -e 'n\n\n\n\n\nw\n' | gdisk %s"
                           device device))

    ;; Encrypt
    (let ((partition (concat device "1")))
      (cf/linux--run (format "cryptsetup luksFormat %s" partition))
      (cf/linux--run (format "cryptsetup open %s %s" partition luks-name))
      (cf/linux--run (format "mkfs.%s /dev/mapper/%s" fs luks-name))
      (cf/linux--run "mount /dev/mapper/cryptroot /mnt"))

    ;; Install base
    (let ((init-pkg (cdr (assoc init cf/linux-init-systems))))
      (cf/linux--run
       (format "%sstrap /mnt base base-devel %s elogind-%s linux linux-firmware"
               (if (string= distro "Artix") "basestrap" "pacstrap")
               init-pkg init-pkg)))

    (cf/linux--run "genfstab -U /mnt > /mnt/etc/fstab")
    (message "Base system installed. Continue setup with chroot.")
    (cf/linux--run "arch-chroot /mnt"))
  (message "Linux installation complete."))



;;; _
(provide 'cf/linux-installer)
;; Local Variables:
;; coding: utf-8
;; fill-column: 90
;; indent-tabs-mode: nil
;; require-final-newline: t
;; End:
;;; cf-linux-installer.el ends here
