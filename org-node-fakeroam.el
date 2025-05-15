;;; org-node-fakeroam.el --- Deprecated extension to org-node -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Martin Edström

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; Author:   Martin Edström <meedstrom91@gmail.com>
;; URL:      https://github.com/meedstrom/org-node-fakeroam
;; Created:  2024-04-13
;; Keywords: org, hypermedia
;; Package-Requires: ((emacs "29.1") (org-mem "0.8.2") (org-node "3.0.0") (org-roam "2.2.2"))

;;; Commentary:

;; Provide stand-ins for `org-roam-autosync-mode' that make use of org-node.

;; Since v3, this package is on life support.

;;; Code:

(require 'cl-lib)
(require 'ol)
(require 'org-macs)
(require 'org-node)
(require 'org-roam)
(require 'org-mem)
(require 'org-mem-roamy)
(declare-function org-roam-dailies--capture "org-roam-dailies")
(declare-function org-node-seq--add-item "org-node-seq")
(declare-function org-roam-db--close-all "org-roam-db")

(unless (fboundp 'org-mem-roamy-db)
  (display-warning 'org-node-fakeroam
                   "org-node-fakeroam v3 has new dependency org-mem.el"))

(unless (fboundp 'org-node--wipe-completions)
  (display-warning 'org-node-fakeroam
                   "org-node-fakeroam v3 depends on org-node v3"))

;;;###autoload
(defun org-node-fakeroam-placeholder-indicating-v3 ())


;;;; Utils

;;;###autoload
(define-obsolete-function-alias 'org-node-fakeroam-new-via-roam-capture
  #'org-node-new-via-roam-capture "2025-05-12")

;;;###autoload
(defun org-node-fakeroam-slugify-via-roam (title)
  "From TITLE, make a filename slug, using org-roam code to do it.

See also the normally equivalent `org-node-slugify-like-roam-default'.
This function only exists in case you had patched the definition of
`org-roam-node-slug' and want to continue using your custom definition."
  (org-roam-node-slug (org-roam-node-create :title title)))

;; TODO: Somehow make `org-node-fakeroam-new-via-roam-capture' able to do this?
;;;###autoload
(defun org-node-fakeroam-daily-create (ymd seq-key &optional goto keys)
  "Create a daily-note, for a day implied by YMD.
YMD must be a time string in YYYY-MM-DD form.

SEQ-KEY is the key that corresponds to the member of `org-node-seq-defs'
that should grow with the captured item after the capture is done.

GOTO and KEYS like in `org-roam-dailies--capture'."
  (require 'org-roam-dailies)
  (add-hook 'org-roam-capture-new-node-hook #'org-node-seq--add-item 90)
  (setq org-node-proposed-seq seq-key)
  (unwind-protect
      (org-roam-dailies--capture
       (encode-time
        (parse-time-string (concat ymd (format-time-string " %T %z"))))
       goto keys)
    (remove-hook 'org-roam-capture-new-node-hook #'org-node-seq--add-item)
    (setq org-node-proposed-seq nil)))


;;;; Bonus commands

;;;###autoload
(define-minor-mode org-node-fakeroam-redisplay-mode
  "Make the Roam buffer react when point moves in any Org buffer.

Normally, `org-roam-db-autosync-mode' sets this up for you - this
mode exists for people who prefer to turn that off.

See also `org-node-fakeroam-fast-render-mode'."
  :global t
  (if org-node-fakeroam-redisplay-mode
      (progn
        (add-hook 'org-mode-hook #'org-roam-buffer--setup-redisplay-h)
        (dolist (buf (org-buffer-list 'files t))
          (with-current-buffer buf
            (add-hook 'post-command-hook #'org-roam-buffer--redisplay-h nil t))))
    (remove-hook 'org-mode-hook #'org-roam-buffer--setup-redisplay-h)
    (unless (and org-roam-db-autosync-mode
                 (member #'org-roam-buffer--setup-redisplay-h
                         org-roam-find-file-hook))
      (dolist (buf (org-buffer-list))
        (with-current-buffer buf
          (remove-hook 'post-command-hook #'org-roam-buffer--redisplay-h t))))))

;;;###autoload
(defun org-node-fakeroam-show-buffer ()
  "Display an org-roam buffer xor refresh an already visible one.

To reiterate: if it was not visible, only bring it up for
display, do NOT also refresh it.  Leave that for the second time
the user invokes the command."
  (interactive)
  (if (derived-mode-p 'org-roam-mode)
      (org-roam-buffer-refresh)
    (pcase (org-roam-buffer--visibility)
      ('visible (if (derived-mode-p 'org-mode)
                    (org-roam-buffer-persistent-redisplay)
                  (with-current-buffer org-roam-buffer
                    (org-roam-buffer-refresh))))
      ('none (when (derived-mode-p 'org-mode)
               (display-buffer (get-buffer-create org-roam-buffer))
               (org-roam-buffer-persistent-redisplay)))
      ('exists (let ((display-buffer-overriding-action
                      '((display-buffer-in-previous-window
                         display-buffer-pop-up-window)
                        (inhibit-same-window . t))))
                 (display-buffer org-roam-buffer))))))

;; (advice-add 'toggle-window-dedicated :after #'org-node-fakeroam-dedicate-same)
(defun org-node-fakeroam-dedicate-same (&rest args)
  "If window is dedicated, set org-roam buffer dedication as well.
Do nothing if the window does not show an org-roam buffer.

Designed as after-advice for Emacs 30 `toggle-window-dedicated'.

Can also be after-advice on any function that toggles dedication.
If that function operates on a window that is not the selected one, the
the window should be the first argument in ARGS."
  (let* ((wd (window-normalize-window
              (when (window-valid-p (car args)) (car args))))
         (buf (window-buffer wd)))
    (when (org-roam-buffer-p buf)
      (if (window-dedicated-p wd)
          ;; Just became dedicated
          (with-current-buffer buf
            (rename-buffer (org-roam-buffer--dedicated-name
                            org-roam-buffer-current-node)))
        ;; Just became undedicated
        (when (and (not (string= org-roam-buffer (buffer-name buf)))
                   (get-buffer org-roam-buffer))
          (kill-buffer org-roam-buffer))
        (with-current-buffer buf
          (rename-buffer org-roam-buffer))))))

;; DEPRECATED
;;;###autoload
(define-obsolete-function-alias 'org-node-fakeroam-fast-render-mode
  #'org-node-roam-accelerator-mode "2025-05-12")

;; DEPRECATED
;;;###autoload
(define-obsolete-function-alias 'org-node-fakeroam-jit-backlinks-mode
  #'org-mem-roamy-jit-backlinks-mode "2025-05-12")


;;;; Shim to feed data to the DB
;; DEPRECATED

;;;###autoload
(defun org-node-fakeroam-db-feed-mode (&rest _)
  "Supply data to the org-roam SQLite database on save."
  (interactive)
  (when org-node-fakeroam-db-feed-mode
    (add-hook 'kill-emacs-hook #'org-roam-db--close-all)
    (unless (and org-mem-roamy-do-overwrite-real-db
                 org-mem-roam-db-mode
                 org-mem-updater-mode
                 (not org-roam-db-update-on-save))
      (setq org-mem-roamy-do-overwrite-real-db t)
      (setq org-roam-db-update-on-save nil)
      (org-mem-roamy-db-mode)
      (org-mem-updater-mode)
      (org-node-fakeroam-db-feed-mode 0)
      (message "org-node-fakeroam-db-feed-mode: This mode is obsolete.
Autoconfiguring `org-mem-roamy-db-mode' for you."))))


;;;; Legacy

(defvar org-node-fakeroam-dir nil
  "Cached value of `org-roam-directory' transformed for org-node.
This path should be directly comparable to the paths saved in
org-node objects, which lets you skip using `file-truename' and
`abbreviate-file-name' to compare paths.

See also `org-node-fakeroam-daily-dir'.")

(defvar org-node-fakeroam-daily-dir nil
  "Cached value for Roam's dailies dir transformed for org-node.
This path should be directly comparable to the paths saved in
org-node objects, which lets you skip using `file-truename' and
`abbreviate-file-name' to compare paths.

Extra rationale: The original `org-roam-dailies-directory' is a
relative path, which incurred verbosity penalties in all code
that used it \(plus a practical performance penalty since
`expand-file-name' was often used instead of `file-name-concat').

Even more verbosity is added on top for org-node, which does a
lot of path comparisons and needs to process the path with
`abbreviate-file-name'.  This variable provides an easy
shorthand.

Will stay nil until sometime after org-roam-dailies is loaded.")

(defun org-node-fakeroam--remember-roam-dirs (&rest _)
  "Cache some convenience variables.
See docstring of `org-node-fakeroam-daily-dir'."
  (when (boundp 'org-roam-directory)
    (setq org-node-fakeroam-dir
          (org-mem--abbrev-file-names
           (file-truename org-roam-directory)))
    (when (boundp 'org-roam-dailies-directory)
      (setq org-node-fakeroam-daily-dir
            (org-mem--abbrev-file-names
             (file-truename
              (if (file-name-absolute-p org-roam-dailies-directory)
                  org-roam-dailies-directory
                (file-name-concat org-roam-directory
                                  org-roam-dailies-directory))))))))

;; This hook would not be needed if we just `require' org-roam-dailies at the
;; top of this file, but I don't want to force that since that module makes
;; unhygienic changes to Emacs on load.
(org-node-fakeroam--remember-roam-dirs)
(add-hook 'org-mem--pre-full-scan-functions
          #'org-node-fakeroam--remember-roam-dirs)
(add-hook 'org-mem--pre-targeted-scan-functions
          #'org-node-fakeroam--remember-roam-dirs)

;; (benchmark-call (byte-compile #'org-roam-list-files))
;; (benchmark-call (byte-compile #'org-node-fakeroam-list-files))
(defun org-node-fakeroam-list-files ()
  "Faster than `org-roam-list-files'."
  (cl-loop for file in (org-mem-org-files)
           when (string-prefix-p org-node-fakeroam-dir file)
           collect file))

;; (benchmark-call (byte-compile #'org-roam-dailies--list-files) 10)
;; (benchmark-call (byte-compile #'org-node-fakeroam-list-dailies) 10)
(defun org-node-fakeroam-list-dailies (&rest extra-files)
  "May be faster than `org-roam-dailies--list-files'.
Makes little difference if your filesystem is not a bottleneck.

For argument EXTRA-FILES, see that function."
  (append extra-files
          (cl-loop
           for file in (org-mem-org-files)
           when (string-prefix-p org-node-fakeroam-daily-dir file)
           collect file)))

;; (benchmark-call (byte-compile #'org-roam-dailies--daily-note-p) 1000)
;; (benchmark-call (byte-compile #'org-node-fakeroam-daily-note-p) 1000)
(defun org-node-fakeroam-daily-note-p (&optional file)
  "May be faster than `org-roam-dailies--daily-note-p'.
Makes little difference if your filesystem is not a bottleneck.

For argument FILE, see that function.

Does not run `file-truename', so not reliable if your Emacs
allows variable `buffer-file-name' to be a symlink."
  (setq file (org-mem--abbrev-file-names
              (or file (buffer-file-name (buffer-base-buffer)))))
  (and (string-suffix-p ".org" file)
       (string-prefix-p (downcase org-node-fakeroam-daily-dir)
                        (downcase file))
       (cl-loop for exclude in org-mem-org-dirs-exclude
                never (string-search exclude file))))

(provide 'org-node-fakeroam)

;;; org-node-fakeroam.el ends here
