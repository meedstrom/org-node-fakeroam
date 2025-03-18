;;; org-node-fakeroam.el --- Stand-ins for org-roam-autosync-mode -*- lexical-binding: t; -*-

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
;; Package-Requires: ((emacs "29.1") (indexed "0.4.1") (org-node "2.3.2") (emacsql "4.0.3") (org-roam "2.2.2"))

;; NOTE: Looking for Package-Version?  Consult the Git tag.
;;       2.0.0 was released on 20250303, i.e. March 3.
;;       3.0.0 was released on 20250324, i.e. March 24.

;;; Commentary:

;; Provide stand-ins for `org-roam-autosync-mode' that make use of org-node.

;;; Code:

(require 'cl-lib)
(require 'ol)
(require 'org-macs)
(require 'org-node)
(require 'org-node-changes)
(require 'org-roam)
(require 'org-roam-db)
(require 'indexed)
(require 'indexed-roam)
(require 'emacsql)
(declare-function org-roam-dailies--capture "org-roam-dailies")
(declare-function org-node-seq--add-item "org-node-seq")

(unless (fboundp 'indexed-roam)
  (display-warning 'org-node-fakeroam
                   "org-node-fakeroam v3 has new dependency indexed.el"))

(unless (fboundp 'org-node--work-buffer-for)
  (display-warning 'org-node-fakeroam
                   "org-node-fakeroam v3 depends on org-node v2.3.2+"))

(unless (fboundp 'org-node-context-toggle)
  (display-warning 'org-node-fakeroam
                   "org-node-fakeroam v2.0 depends on org-node v2.0"))

(unless (fboundp 'org-node-seq-dispatch)
  (display-warning 'org-node-fakeroam
                   "org-node-fakeroam v1.7 depends on org-node v1.9"))


;;;; Utils

(defgroup org-node-fakeroam nil
  "Shims for org-roam."
  :group 'org-node)

;;;###autoload
(defun org-node-fakeroam-new-via-roam-capture ()
  "Call `org-roam-capture-' with predetermined arguments.
Meant to be called indirectly as `org-node-creation-fn', at which
time some necessary variables are set."
  (when (or (null org-node-proposed-title)
            (null org-node-proposed-id))
    (error "`org-node-fakeroam-new-via-roam-capture' is meant to be called indirectly via `org-node-create'"))
  (unless (require 'org-roam nil t)
    (error "`org-node-fakeroam-new-via-roam-capture' requires library \"org-roam\""))
  (when (and (fboundp 'org-roam-capture-)
             (fboundp 'org-roam-node-create))
    (org-roam-capture- :node (org-roam-node-create
                              :title org-node-proposed-title
                              :id    org-node-proposed-id))))

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


;;;; Fast Render Mode

(defvar org-node-fakeroam--id<>previews (make-hash-table :test #'equal)
  "1:N table mapping IDs to seen previews of backlink contexts.
For use by `org-node-fakeroam-fast-render-mode'.

Each preview is a cons cell \(POS-DIFF . TEXT) where POS-DIFF
corresponds to a link\\='s buffer position relative to that of
the heading that has said ID, and TEXT is an output of
`org-roam-preview-get-contents'.")

;;;###autoload
(define-minor-mode org-node-fakeroam-fast-render-mode
  "Advise the Roam buffer to be faster.

1. Make the buffer build faster, details in docstring
   `org-node-fakeroam--fast-render-get-contents'.

2. Cache the previews, so that there is less or no lag the next
   time the same nodes are visited."
  :global t
  (if org-node-fakeroam-fast-render-mode
      (progn
        (advice-add 'org-roam-node-insert-section :around    #'org-node-fakeroam--fast-render-inhibit-fontifying)
        (advice-add 'org-roam-preview-get-contents :override #'org-node-fakeroam--fast-render-get-contents)
        (advice-add 'org-roam-buffer-render-contents :after  #'org-node--kill-work-buffers))
    (advice-remove 'org-roam-node-insert-section    #'org-node-fakeroam--fast-render-inhibit-fontifying)
    (advice-remove 'org-roam-preview-get-contents   #'org-node-fakeroam--fast-render-get-contents)
    (advice-remove 'org-roam-buffer-render-contents #'org-node--kill-work-buffers)))

(defvar org-node-fakeroam--fast-render-src-roam-node nil)
(defun org-node-fakeroam--fast-render-inhibit-fontifying (orig-fn &rest args)
  "Intended as around-advice for `org-roam-node-insert-section'.

Run ORIG-FN with ARGS, while overriding
`org-roam-fontify-like-in-org-mode' so it does nothing.

While we\\='re at it, inspect ARGS for its SOURCE-NODE argument and
store it in the variable `org-node-fakeroam--fast-render-src-roam-node'."
  (setq org-node-fakeroam--fast-render-src-roam-node
        (plist-get args :source-node))
  (cl-letf (((symbol-function 'org-roam-fontify-like-in-org-mode) #'identity))
    (apply orig-fn args)))

(defun org-node-fakeroam--fast-render-get-contents (file link-pos)
  "Designed as override for `org-roam-preview-get-contents'.
Get a preview snippet out of FILE at position LINK-POS, letting
`org-roam-preview-function' determine contents of snippet.

Normally the first time you open an org-roam buffer, Emacs hangs for as
long as a minute on a slow machine when huge files are involved, due to
having to fontify each file\\='s entire contents in a hidden buffer,
then applying #+startup:indent and other options, then org-element cache
has to do its thing even though the cache will be thrown away.

This tries to eliminate all those problems.

Aside from huge files, it is also slow when there are backlinks coming
from many sources.  To deal with that:

1. this reuses buffers if several backlinks originate from the same file
2. even if all originate from different files, this caches all snippets
   so that it should only be slow the first time."
  (unless org-node-fakeroam--fast-render-src-roam-node
    (error "org-node-fakeroam: No SOURCE-NODE passed"))
  (let* ((src-id (org-roam-node-id org-node-fakeroam--fast-render-src-roam-node))
         (src-node (gethash src-id indexed--id<>entry))
         (_ (unless src-node
              (error "Roam node unknown to Org-node: %s" src-id)))
         ;; NOTE: `pos-diff' is not necessary in a naive implementation, but
         ;; this level of granularity lets us avoid wiping all cached previews
         ;; for a large file every time it is saved -- doing so would make the
         ;; cache useless every time it is actually needed, when you are
         ;; working in a large file with links between parts of itself.
         ;;
         ;; Instead, we just don't wipe anything, and trust in a sloppy rule of
         ;; thumb: when the text between a link and its heading get edited,
         ;; that will almost always result in a new unique `pos-diff'
         ;; (especially if it was meaningfully edited!).
         (pos-diff (- link-pos (indexed-pos src-node))))
    (or (alist-get pos-diff (gethash src-id org-node-fakeroam--id<>previews))
        ;; No cached preview, cache a new one
        (setf
         (alist-get pos-diff (gethash src-id org-node-fakeroam--id<>previews))
         (let ((org-inhibit-startup t)
               (org-element-cache-persistent nil)
               snippet)
           (with-current-buffer (org-node--work-buffer-for file)
             (goto-char link-pos)
             (cl-letf (((symbol-function 'org-back-to-heading-or-point-min)
                        #'org-node--back-to-heading-or-point-min))
               (setq snippet (funcall org-roam-preview-function))
               (dolist (fn org-roam-preview-postprocess-functions)
                 (setq snippet (funcall fn snippet)))))
           (with-current-buffer (org-node--general-org-work-buffer)
             (erase-buffer)
             (insert snippet)
             (font-lock-ensure)
             (buffer-string)))))))


;;;; JIT backlinks sidestepping the database
;; DEPRECATED

;;;###autoload
(define-minor-mode org-node-fakeroam-jit-backlinks-mode
  "Override org-roam backlink-getters to look up org-node tables.

As a result, \\[org-roam-buffer-toggle] will function without
having SQLite installed."
  :global t
  (if org-node-fakeroam-jit-backlinks-mode
      (progn
        (unless indexed-updater-mode
          (message "`org-node-fakeroam-jit-backlinks-mode' needs `indexed-updater-mode'"))
        (advice-add 'org-roam-backlinks-get :override #'indexed-roam-mk-backlinks)
        (advice-add 'org-roam-reflinks-get  :override #'indexed-roam-mk-reflinks))
    (advice-remove 'org-roam-backlinks-get #'indexed-roam-mk-backlinks)
    (advice-remove 'org-roam-reflinks-get  #'indexed-roam-mk-reflinks)))


;;;; Shim to feed data to the DB
;; DEPRECATED

;;;###autoload
(defun org-node-fakeroam-db-feed-mode (&rest _)
  "Supply data to the org-roam SQLite database on save."
  (interactive)
  (add-hook 'kill-emacs-hook #'org-roam-db--close-all)
  (unless (and indexed-roam-overwrite
               indexed-roam-mode
               indexed-updater-mode
               (not org-roam-db-update-on-save))
    (setq indexed-roam-overwrite t)
    (setq org-roam-db-update-on-save nil)
    (indexed-roam-mode)
    (indexed-updater-mode)
    (message "org-node-fakeroam-db-feed-mode: This mode is obsolete.
To have the same effect, configuring `indexed-roam-mode' for you.")))


;;;; Bonus advices

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
          (indexed--abbrev-file-names
           (file-truename org-roam-directory)))
    (when (boundp 'org-roam-dailies-directory)
      (setq org-node-fakeroam-daily-dir
            (indexed--abbrev-file-names
             (file-truename
              (if (file-name-absolute-p org-roam-dailies-directory)
                  org-roam-dailies-directory
                (file-name-concat org-roam-directory
                                  org-roam-dailies-directory))))))))

;; This hook would not be needed if we just `require' org-roam-dailies at the
;; top of this file, but I don't want to force that since that module makes
;; unhygienic changes to Emacs on load.
(org-node-fakeroam--remember-roam-dirs)
(add-hook 'indexed--pre-full-reset-functions
          #'org-node-fakeroam--remember-roam-dirs)
(add-hook 'indexed--pre-incremental-update-functions
          #'org-node-fakeroam--remember-roam-dirs)

;; (benchmark-call (byte-compile #'org-roam-list-files))
;; (benchmark-call (byte-compile #'org-node-fakeroam-list-files))
(defun org-node-fakeroam-list-files ()
  "Faster than `org-roam-list-files'."
  (cl-loop for file in (indexed-org-files)
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
           for file in (indexed-org-files)
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
  (setq file (indexed--abbrev-file-names
              (or file (buffer-file-name (buffer-base-buffer)))))
  (and (string-suffix-p ".org" file)
       (string-prefix-p (downcase org-node-fakeroam-daily-dir)
                        (downcase file))
       (cl-loop for exclude in indexed-org-dirs-exclude
                never (string-search exclude file))))

(provide 'org-node-fakeroam)

;;; org-node-fakeroam.el ends here
