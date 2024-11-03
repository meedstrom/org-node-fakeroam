;;; org-node-fakeroam.el --- Stand-ins for org-roam-autosync-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Martin Edström

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

;; Author:           Martin Edström <meedstrom91@gmail.com>
;; Created:          2024-04-13
;; Keywords:         org, hypermedia
;; URL:              https://github.com/meedstrom/org-node-fakeroam
;; Package-Requires: ((emacs "28.1") (compat "30") (org-node "1.5.10") (org-roam "2.2.2") (emacsql "4.0.3"))

;;; Commentary:

;; Provide stand-ins for `org-roam-autosync-mode' that make use of org-node.

;;; Code:

(require 'cl-lib)
(require 'ol)
(require 'org-node)
(require 'org-node-changes)
(require 'org-node-parser)
(require 'org-roam)
(require 'org-roam-db)
(require 'emacsql)

(declare-function org-roam-dailies--capture "org-roam-dailies")

(unless (and (fboundp 'org-node-parser--tmpfile)
             (not (fboundp 'org-node--write-eld))
             (fboundp 'org-node-get-reflinks-to))
  (display-warning 'org-node-fakeroam
                   "Fakeroam v1.3 depends on org-node v1.5.x"))


;;;; Utils

;;;###autoload
(defun org-node-fakeroam-new-via-roam-capture ()
  "Call `org-roam-capture-' with predetermined arguments.
Meant to be called indirectly as `org-node-creation-fn', at which
time some necessary variables are set."
  (when (or (null org-node-proposed-title)
            (null org-node-proposed-id))
    (error "`org-node-fakeroam-new-via-roam-capture' is meant to be called indirectly via `org-node-create'"))
  (org-roam-capture- :node (org-roam-node-create
                            :title org-node-proposed-title
                            :id    org-node-proposed-id)))

;;;###autoload
(defun org-node-fakeroam-slugify-via-roam (title)
  "From TITLE, make a filename slug, using Roam code to do it.

See also the equivalent `org-node-slugify-like-roam-default'.  This
function only exists in case you had patched the definition of
`org-roam-node-slug' and want to continue using your custom definition."
  (org-roam-node-slug (org-roam-node-create :title title)))


;;;; Roam buffer hacks

;;;###autoload
(define-minor-mode org-node-fakeroam-redisplay-mode
  "Make the Roam buffer react when point moves in any Org buffer.

Normally, `org-roam-db-autosync-mode' sets this up for you - this
mode exists for people who prefer to turn that off.

See also `org-node-fakeroam-fast-render-mode'.

-----"
  :global t
  :group 'org-node
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

(defvar org-node-fakeroam--id<>previews (make-hash-table :test #'equal)
  "1:N table mapping IDs to seen previews of backlink contexts.
For use by `org-node-fakeroam-fast-render-mode'.

Each preview is a cons cell \(POS-DIFF . TEXT) where POS-DIFF
corresponds to a link\\='s buffer position relative to that of
the heading that has said ID, and TEXT is an output of
`org-roam-preview-get-contents'.")

(defcustom org-node-fakeroam-previews-file
  ;; TODO: Should probably just do a PR to
  ;;       https://github.com/emacscollective/no-littering
  (file-name-concat (or (bound-and-true-p no-littering-var-directory)
                        user-emacs-directory)
                    "org-node-fakeroam-cached-previews.eld")
  "File containing cached backlink previews.
Stores the value of table `org-node-fakeroam--id<>previews' between
sessions.

Only meaningful if `org-node-fakeroam-persist-previews' is non-nil."
  :group 'org-node
  :type 'file)

(defcustom org-node-fakeroam-persist-previews nil
  "Whether to sync backlink previews to disk.

Only meaningful with `org-node-fakeroam-fast-render-mode' active.

Keep in mind it would store potentially world-readable note contents at
`org-node-fakeroam-previews-file'.  That may be moot at the moment, as
org-node itself does the same under /tmp (or variable
`temporary-file-directory')."
  :group 'org-node
  :type 'boolean)

(defvar org-node-fakeroam--last-hash (sxhash org-node-fakeroam--id<>previews))
(defun org-node-fakeroam--fast-render-persist ()
  "Sync cached previews to disk."
  (if org-node-fakeroam-persist-previews
      ;; Only proceed if table has grown
      (when (/= org-node-fakeroam--last-hash
                (sxhash org-node-fakeroam--id<>previews))
        (org-node-fakeroam--clean-stale-previews)
        (setq org-node-fakeroam--last-hash
              (sxhash org-node-fakeroam--id<>previews))
        (let ((buf (find-buffer-visiting org-node-fakeroam-previews-file)))
          (when buf (kill-buffer buf))
          (write-region (prin1-to-string org-node-fakeroam--id<>previews
                                         nil
                                         '((length . nil) (level . nil)))
                        nil
                        org-node-fakeroam-previews-file
                        nil
                        'quiet)
          (when buf (find-file-noselect org-node-fakeroam-previews-file))))
    (cancel-timer org-node-fakeroam--persistence-timer)
    (setq org-node-fakeroam--did-setup-persistence nil)))

(defun org-node-fakeroam--clean-stale-previews ()
  "Clean stale members of table `org-node-fakeroam--id<>previews'."
  (let ((valid-positions (make-hash-table :test #'equal)))
    (cl-loop for links being each hash-value of org-node--dest<>links do
             (dolist (link links)
               (push (org-node-link-pos link)
                     (gethash (org-node-link-origin link) valid-positions))))
    (maphash (lambda (id previews)
               (if-let ((node (gethash id org-node--id<>node)))
                   (cl-loop for (pos-diff . _text) in previews
                            unless (memq (+ (org-node-get-pos node) pos-diff)
                                         (gethash id valid-positions))
                            do (remhash id org-node-fakeroam--id<>previews))
                 (remhash id org-node-fakeroam--id<>previews)))
             org-node-fakeroam--id<>previews)))

(defvar org-node-fakeroam--persistence-timer (timer-create))
(defvar org-node-fakeroam--did-setup-persistence nil)
(defun org-node-fakeroam--try-setup-persistence (&rest _)
  "Try to restore `org-node-fakeroam--id<>previews' from disk.
Then start intermittently syncing to disk."
  (when (and org-node-fakeroam-persist-previews
             (not org-node-fakeroam--did-setup-persistence))
    (setq org-node-fakeroam--did-setup-persistence t)
    (cancel-timer org-node-fakeroam--persistence-timer)
    (setq org-node-fakeroam--persistence-timer
          (run-with-idle-timer 60 t #'org-node-fakeroam--fast-render-persist))
    (when (file-readable-p org-node-fakeroam-previews-file)
      ;; Load from disk
      (with-temp-buffer
        (insert-file-contents org-node-fakeroam-previews-file)
        (when-let ((data (ignore-errors
                           (car (read-from-string (buffer-string))))))
          (when (hash-table-p data)
            (setq org-node-fakeroam--id<>previews data)))))))

;;;###autoload
(define-minor-mode org-node-fakeroam-fast-render-mode
  "Advise the Roam buffer to be faster.

1. Make the buffer build faster by nullifying certain Org options
   inside the context previews.

2. Cache the previews, so that there is less or no lag the next
   time the same nodes are visited.

See also `org-node-fakeroam-persist-previews' if you have a particularly
slow filesystem or CPU, or often see dozens of backlinks originating
from large files.

-----"
  :global t
  :group 'org-node
  (if org-node-fakeroam-fast-render-mode
      (progn
        (advice-add #'org-roam-buffer-render-contents :before
                    #'org-node-fakeroam--try-setup-persistence)
        (advice-add #'org-roam-node-insert-section :around
                    #'org-node-fakeroam--run-without-fontifying)
        (advice-add #'org-roam-preview-get-contents :around
                    #'org-node-fakeroam--accelerate-get-contents))
    (cancel-timer org-node-fakeroam--persistence-timer)
    (advice-remove #'org-roam-buffer-render-contents
                   #'org-node-fakeroam--try-setup-persistence)
    (advice-remove #'org-roam-node-insert-section
                   #'org-node-fakeroam--run-without-fontifying)
    (advice-remove #'org-roam-preview-get-contents
                   #'org-node-fakeroam--accelerate-get-contents)))

(defvar org-node-fakeroam--src-roam-node nil)
(defun org-node-fakeroam--run-without-fontifying (orig-fn &rest args)
  "Intended as around-advice for `org-roam-node-insert-section'.

Inspect ARGS for its SOURCE-NODE argument and store
it in the variable `org-node-fakeroam--src-roam-node'.

Then run ORIG-FN with ARGS, while overriding
`org-roam-fontify-like-in-org-mode' so it does nothing."
  (setq org-node-fakeroam--src-roam-node (plist-get args :source-node))
  (cl-letf (((symbol-function 'org-roam-fontify-like-in-org-mode) #'identity))
    (apply orig-fn args)))

(defun org-node-fakeroam--accelerate-get-contents (orig-fn file pt)
  "Designed as around-advice for `org-roam-preview-get-contents'.

Normally the first time you open a Roam buffer, Emacs hangs
for as long as a minute on a slow machine when huge files are
involved.  This may eliminate most of that.

Aside from huge files, it is also slow when there are backlinks
coming from from extremely many files.  To deal with that, this
caches all results so that it should only be slow the first time.

Argument ORIG-FN is presumably `org-roam-preview-get-contents',
which expects arguments FILE and PT, where PT is the buffer
position of a link."
  (unless org-node-fakeroam--src-roam-node
    (error "org-node-fakeroam: No SOURCE-NODE passed"))
  (let* ((src-id (org-roam-node-id org-node-fakeroam--src-roam-node))
         (src-node (gethash src-id org-node--id<>node))
         (pos-diff (if src-node
                       (- pt (org-node-get-pos src-node))
                     (error "Roam node unknown to Org-node: %s" src-id))))
    (or (alist-get pos-diff (gethash src-id org-node-fakeroam--id<>previews))
        (setf
         (alist-get pos-diff (gethash src-id org-node-fakeroam--id<>previews))
         (let ((org-inhibit-startup t))
           (delay-mode-hooks
             ;; NOTE: We cannot use `org-roam-fontify-like-in-org-mode'
             ;;       since it is temporarily overridden.
             (org-fontify-like-in-org-mode (funcall orig-fn file pt))))))))

;; Just a bonus command
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


;;;; Backlinks: JIT shim
;; Fabricate knockoff Roam backlinks in real time, so that no DB is needed
;; for displaying the Roam buffer

;;;###autoload
(define-minor-mode org-node-fakeroam-jit-backlinks-mode
  "Override org-roam backlink-getters to look up org-node tables.

As a result, \\[org-roam-buffer-toggle] will function without
having SQLite installed, and you can delete org-roam.db if you do
not need it for other things.

-----"
  :global t
  :group 'org-node
  (if org-node-fakeroam-jit-backlinks-mode
      (progn
        (unless org-node-cache-mode
          (message "`org-node-fakeroam-jit-backlinks-mode' will do poorly without `org-node-cache-mode'"))
        (advice-add 'org-roam-backlinks-get :override #'org-node-fakeroam--mk-backlinks)
        (advice-add 'org-roam-reflinks-get  :override #'org-node-fakeroam--mk-reflinks))
    (advice-remove 'org-roam-backlinks-get #'org-node-fakeroam--mk-backlinks)
    (advice-remove 'org-roam-reflinks-get  #'org-node-fakeroam--mk-reflinks)))

(defun org-node-fakeroam--mk-node (node)
  "Make an org-roam-node object from org-node object NODE."
  (org-roam-node-create
   :file (org-node-get-file-path node)
   :id (org-node-get-id node)
   :olp (org-node-get-olp node)
   :scheduled (when-let ((scheduled (org-node-get-scheduled node)))
                (format-time-string
                 "%FT%T%z"
                 (encode-time (org-parse-time-string scheduled))))
   :deadline (when-let ((deadline (org-node-get-deadline node)))
               (format-time-string
                "%FT%T%z"
                (encode-time (org-parse-time-string deadline))))
   :level (org-node-get-level node)
   :title (org-node-get-title node)
   :file-title (org-node-get-file-title-or-basename node)
   :tags (org-node-get-tags-with-inheritance node)
   :aliases (org-node-get-aliases node)
   :todo (org-node-get-todo node)
   :refs (org-node-get-refs node)
   :point (org-node-get-pos node)
   :priority (org-node-get-priority node)
   :properties (org-node-get-properties node)))

(defun org-node-fakeroam--mk-backlinks (target-roam-node &rest _)
  "Make org-roam-backlink objects pointing to TARGET-ROAM-NODE.
Designed to override `org-roam-backlinks-get'."
  (let* ((target-id (org-roam-node-id target-roam-node))
         (links (gethash target-id org-node--dest<>links)))
    (cl-loop
     for link in links
     as src-id = (org-node-link-origin link)
     as src-node = (gethash src-id org-node--id<>node)
     when src-node
     collect (org-roam-backlink-create
              :target-node target-roam-node
              :source-node (org-node-fakeroam--mk-node src-node)
              :point (org-node-link-pos link)
              :properties
              (list :outline
                    (append (org-node-get-olp src-node)
                            (list (org-node-get-title src-node))))))))

(defun org-node-fakeroam--mk-reflinks (target-roam-node &rest _)
  "Make org-roam-reflink objects pointing to TARGET-ROAM-NODE.
Designed to override `org-roam-reflinks-get'."
  (let* ((target-id (org-roam-node-id target-roam-node))
         (node (gethash target-id org-node--id<>node)))
    (when node
      (cl-loop
       for ref in (org-node-get-refs node)
       append (cl-loop
               for link in (gethash ref org-node--dest<>links)
               as src-id = (org-node-link-origin link)
               as src-node = (gethash src-id org-node--id<>node)
               when src-node
               collect (org-roam-reflink-create
                        :ref (org-node-link-dest link)
                        :source-node (org-node-fakeroam--mk-node src-node)
                        :point (org-node-link-pos link)
                        :properties
                        (list :outline
                              (append (org-node-get-olp src-node)
                                      (list (org-node-get-title src-node))))))))))


;;;; Backlinks: Feed-the-db shim

(defvar org-node-fakeroam--orig-db-loc nil)
(defvar org-node-fakeroam--overwrite-db-timer (timer-create))

;; FIXME: Leaves a bit too many /tmp/.../org-roam.X.db files when restarting
;;        often

;;;###autoload
(define-minor-mode org-node-fakeroam-db-feed-mode
  "Supply data to the org-roam SQLite database on save.

Actually, reassign `org-roam-db-location' to an unique temporary
file name and write to that one for as long as the mode is
active, and intermittently merge the temporary file with the original.

-----"
  :global t
  :group 'org-node
  (org-roam-db--close-all)
  (cancel-timer org-node-fakeroam--overwrite-db-timer)

  (if org-node-fakeroam-db-feed-mode
      (progn
        (when (and org-roam-db-update-on-save
                   org-roam-db-autosync-mode)
          (message "org-node-fakeroam: You probably want to set `org-roam-db-update-on-save' to nil"))
        (unless org-node-cache-mode
          (message "`org-node-fakeroam-db-feed-mode' will do nothing without `org-node-cache-mode'"))
        (unless (file-writable-p org-roam-db-location)
          (error "`org-roam-db-location' not writable: %s"
                 org-roam-db-location))
        (setq org-node-fakeroam--orig-db-loc org-roam-db-location)
        (setq org-roam-db-location (org-node-fakeroam--mk-uniq-db-loc))
        (when (file-readable-p org-node-fakeroam--orig-db-loc)
          (copy-file org-node-fakeroam--orig-db-loc org-roam-db-location))
        (setq org-node-fakeroam--overwrite-db-timer
              (run-with-idle-timer 60 t #'org-node-fakeroam--overwrite-db))
        (advice-add 'org-roam-node-insert-section :filter-args #'org-node-fakeroam--mk-link-props)
        (add-hook 'org-node-rescan-functions #'org-node-fakeroam--update-db)
        (add-hook 'kill-emacs-hook           #'org-node-fakeroam--delete-db)
        (add-hook 'kill-emacs-hook           #'org-roam-db--close-all))

    (when org-node-fakeroam--orig-db-loc
      (delete-file org-roam-db-location)
      (setq org-roam-db-location org-node-fakeroam--orig-db-loc)
      (setq org-node-fakeroam--orig-db-loc nil))
    (advice-remove 'org-roam-node-insert-section #'org-node-fakeroam--mk-link-props)
    (remove-hook 'org-node-rescan-functions      #'org-node-fakeroam--update-db)
    (remove-hook 'kill-emacs-hook                #'org-node-fakeroam--delete-db)
    (unless org-roam-db-autosync-mode
      (remove-hook 'kill-emacs-hook #'org-roam-db--close-all))))

(defun org-node-fakeroam--mk-link-props (args)
  "A :filter-args advice for `org-roam-node-insert-section'.

Modify the ARGS plist so that the third key, :properties, has a
value that looks like \(:outline OUTLINE-PATH-TO-THE-NODE).

This info is trivial to reconstruct from the first key, :source-node,
hence `org-node-fakeroam-db-feed-mode' not including it with the link
metadata sent to the DB.  Such pre-construction would cost much more
compute, as it has to be done for every link inside the buffer being
saved."
  (unless org-node-fakeroam-jit-backlinks-mode ;; Not needed if that is enabled
    (let ((roam-node (plist-get args :source-node)))
      (setf (plist-get args :properties)
            (list :outline
                  (append (org-roam-node-olp roam-node)
                          (list (org-roam-node-title roam-node)))))))
  args)

;; REASONABLE USER STORY:

;; - User uses db-feed-mode
;; - User edits notes in multiple Emacs instances
;; - User powercycles the computer, skipping `kill-emacs-hook'
;; - User starts fresh Emacs
;; - User expects an up-to-date DB

;; OUR ADDITIONAL REQUIREMENT:

;; - Do not access same DB from multiple Emacs instances
;;   because it seems it can slow down SQL queries

;; SOLUTION:

;; 1. During usage, always work with /tmp/.../...X.db, and let an
;;    intermittent timer copy that one to overwrite the real db -- this
;;    survives powercycles.

;; 2. Every time we're about to write to the DB, check if the other emacsen's
;;    DB copies are newer, and copy the newest one to overwrite our local copy.

(defun org-node-fakeroam--delete-db ()
  "Delete `org-roam-db-location'."
  (delete-file org-roam-db-location))

(defun org-node-fakeroam--overwrite-db ()
  "Update the org-roam SQLite DB on disk.
During usage, `org-node-fakeroam-db-feed-mode' actually uses a
temporary file to minimize the performance hit when multiple
instances of Emacs have a connection open.

This function lets the temporary copy overwrite the original."
  (when (and (file-readable-p org-roam-db-location)
             (file-writable-p org-node-fakeroam--orig-db-loc)
             (file-newer-than-file-p org-roam-db-location
                                     org-node-fakeroam--orig-db-loc))
    (copy-file org-roam-db-location org-node-fakeroam--orig-db-loc t)))

(defun org-node-fakeroam--mk-uniq-db-loc ()
  "Return a temporary file ending in .db that does not yet exist."
  (let (path (ctr 0))
    (while (file-exists-p (setq path (org-node-parser--tmpfile
                                      "org-roam.%d.db" (cl-incf ctr)))))
    path))

(defun org-node-fakeroam--check-simultaneous-dbs ()
  "Ensure `org-roam-db-location' has the newest data.

Multiple Emacs instances that enable
`org-node-fakeroam-db-feed-mode' will each have their own DB copy
in a temporary directory, to avoid the performance hit of one DB
being handled by several open EmacSQL connections.

This function lets the newest copy overwrite the current
instance\\='s copy."
  (let ((locs (cl-loop for file in (directory-files (org-node-parser--tmpfile)
                                                    t "org-roam" t)
                       when (string-suffix-p ".db" file)
                       collect file)))
    ;; REVIEW: Maybe include the original DB?
    ;; (and org-node-fakeroam--orig-db-loc
    ;;      (file-readable-p org-node-fakeroam--orig-db-loc)
    ;;      (push org-node-fakeroam--orig-db-loc locs))
    (let ((newest (car (sort locs #'file-newer-than-file-p))))
      (unless (equal newest org-roam-db-location)
        (org-roam-db--close-all)
        (copy-file newest org-roam-db-location t)))))

;; TODO: Was hoping to just run this on every save.  Is SQLite really so slow
;;       to accept 0-5 MB of data?  Must be some way to make it instant.
;; (benchmark-run (org-node-fakeroam-db-rebuild))
;; => (6.463400598 7 1.107884319)
;; (benchmark-run (org-roam-db-sync 'force))
;; => (179.921311207 147 37.955398732)
(defun org-node-fakeroam-db-rebuild ()
  "Wipe the Roam DB and rebuild."
  (interactive)
  (org-node-cache-ensure)
  (org-roam-db--close)
  (delete-file org-roam-db-location)
  (emacsql-with-transaction (org-roam-db)
    (let ((ctr 0)
          (max (hash-table-count org-nodes))
          (already (make-hash-table :test #'equal)))
      (cl-loop for node being the hash-values of org-nodes
               as file = (org-node-get-file-path node)
               do (when (= 0 (% (cl-incf ctr)
                                (cond ((> ctr 200) 100)
                                      ((> ctr 20) 10)
                                      (t 1))))
                    (message "Inserting into %s... %d/%d"
                             org-roam-db-location ctr max))
               (unless (gethash file already)
                 (puthash file t already)
                 (org-node-fakeroam--db-add-file-level-data node))
               (org-node-fakeroam--db-add-node node)))))

;; Purpose-focused alternative to `org-node-fakeroam-db-rebuild'
;; because that is not instant.

;; FIXME: Still too slow on a file with 400 nodes & 3000 links.
;;        Profiler says most of it is in EmacSQL, maybe some SQL PRAGMA
;;        settings would fix?  Or gather all data for one single `emacsql' call?
;;        Or give up and do it async.
(defun org-node-fakeroam--update-db (files)
  "Update the Roam DB about nodes and links involving FILES."
  (org-node-fakeroam--check-simultaneous-dbs)
  (emacsql-with-transaction (org-roam-db)
    (dolist (file files)
      (org-roam-db-query [:delete :from files :where (= file $s1)]
                         file))
    (let (already)
      (cl-loop
       for node being each hash-value of org-node--id<>node
       as file = (org-node-get-file-path node)
       when (member file files) do
       (unless (member file already)
         (push file already)
         (org-node-fakeroam--db-add-file-level-data node))
       ;; Clear backlinks to prevent duplicates
       ;; TODO: Clear citations too
       (dolist (dest (cons (org-node-get-id node)
                           (org-node-get-refs node)))
         (org-roam-db-query [:delete :from links :where (= dest $s1)]
                            dest))
       (org-node-fakeroam--db-add-node node)))))

(defun org-node-fakeroam--db-add-file-level-data (node)
  "Send metadata about the file where NODE is located."
  (let* ((file (org-node-get-file-path node))
         (lisp-mtime (seconds-to-time
                      (car (gethash file org-node--file<>mtime.elapsed)))))
    ;; See `org-roam-db-insert-file'
    (org-roam-db-query [:insert :into files :values $v1]
                       (vector file
                               (org-node-get-file-title node)
                               ""         ; HACK: Hashing is slow, skip it
                               lisp-mtime ; HACK: Roam doesn't use atime anyway
                               lisp-mtime))))

(defun org-node-fakeroam--db-add-node (node)
  "Send to the SQLite database all we know about NODE.
This includes all links and citations that touch NODE."
  (cl-symbol-macrolet ;; PERF: 20% faster rebuild than with `let'
      ((id         (org-node-get-id node))
       (file-path  (org-node-get-file-path node))
       (tags       (org-node-get-tags-with-inheritance node))
       (aliases    (org-node-get-aliases node))
       (roam-refs  (org-node-get-refs node))
       (title      (org-node-get-title node))
       (properties (org-node-get-properties node)) ;; NOTE: no implicit props!
       (level      (org-node-get-level node))
       (todo       (org-node-get-todo node))
       (scheduled  (org-node-get-scheduled node))
       (deadline   (org-node-get-deadline node))
       (olp        (org-node-get-olp node))
       (priority   (org-node-get-priority node))
       (pos        (org-node-get-pos node)))
    ;; See `org-roam-db-insert-aliases'
    (when aliases
      (org-roam-db-query [:insert :into aliases :values $v1]
                         (cl-loop for alias in aliases
                                  collect (vector id alias))))
    ;; See `org-roam-db-insert-tags'
    (when tags
      (org-roam-db-query [:insert :into tags :values $v1]
                         (cl-loop for tag in tags
                                  collect (vector id tag))))
    ;; See `org-roam-db-insert-file-node' and `org-roam-db-insert-node-data'
    (org-roam-db-query
     [:insert :into nodes :values $v1]
     (vector id
             file-path
             level
             pos
             todo
             priority
             (when scheduled (format-time-string
                              "%FT%T%z"
                              (encode-time (org-parse-time-string scheduled))))
             (when deadline (format-time-string
                             "%FT%T%z"
                             (encode-time (org-parse-time-string deadline))))
             title
             properties
             olp))
    ;; See `org-roam-db-insert-refs'
    (dolist (ref roam-refs)
      (let ((type (gethash ref org-node--ref-path<>ref-type)))
        (org-roam-db-query [:insert :into refs :values $v1]
                           (if type
                               ;; Ref is //www.gnu.org or some such
                               (vector id ref type)
                             ;; Ref is a @citekey
                             (vector id ref "cite")))))

    (let ((dummy-properties '(:outline nil)))
      (dolist (link (nconc (org-node-get-id-links-to node)
                           (org-node-get-reflinks-to node)))
        (if (org-node-link-type link)
            ;; See `org-roam-db-insert-link'
            (org-roam-db-query [:insert :into links :values $v1]
                               (vector (org-node-link-pos link)
                                       (org-node-link-origin link)
                                       (org-node-link-dest link)
                                       (org-node-link-type link)
                                       dummy-properties))
          ;; See `org-roam-db-insert-citation'
          (org-roam-db-query [:insert :into citations :values $v1]
                             (vector (org-node-link-origin link)
                                     (org-node-link-dest link)
                                     (org-node-link-pos link)
                                     dummy-properties)))))))


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

(defun org-node-fakeroam--remember-roam-dirs ()
  "Cache some convenience variables.
See docstring of `org-node-fakeroam-daily-dir'."
  (when (boundp 'org-roam-directory)
    (setq org-node-fakeroam-dir
          (org-node-abbrev-file-names
           (file-truename org-roam-directory)))
    (when (boundp 'org-roam-dailies-directory)
      (setq org-node-fakeroam-daily-dir
            (org-node-abbrev-file-names
             (file-truename
              (if (file-name-absolute-p org-roam-dailies-directory)
                  org-roam-dailies-directory
                (file-name-concat org-roam-directory
                                  org-roam-dailies-directory))))))))

;; This hook would not be needed if we just `require' org-roam-dailies at the
;; top of this file, but I don't want to force that since that module makes
;; unhygienic changes to Emacs on load.
(org-node-fakeroam--remember-roam-dirs)
(add-hook 'org-node-before-update-tables-hook
          #'org-node-fakeroam--remember-roam-dirs)

;; (benchmark-call (byte-compile #'org-roam-list-files))
;; (benchmark-call (byte-compile #'org-node-fakeroam-list-files))
(defun org-node-fakeroam-list-files ()
  "Faster than `org-roam-list-files'."
  (cl-loop for file in (org-node-list-files t)
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
           for file in (org-node-list-files t)
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
  (setq file (org-node-abbrev-file-names
              (or file (buffer-file-name (buffer-base-buffer)))))
  (and (string-suffix-p ".org" file)
       (string-prefix-p (downcase org-node-fakeroam-daily-dir)
                        (downcase file))
       (cl-loop for exclude in org-node-extra-id-dirs-exclude
                never (string-search exclude file))))


;;;; Series-related

;; TODO: Somehow make `org-node-fakeroam-new-via-roam-capture' able to do this?
;;;###autoload
(defun org-node-fakeroam-daily-create (ymd series-key &optional goto keys)
  "Create a daily-note, for a day implied by YMD.
YMD must be a time string in YYYY-MM-DD form.

SERIES-KEY is the key that corresponds to the member of
`org-node-series-defs' that should grow after the capture is
done.

GOTO and KEYS are like in `org-roam-dailies--capture'."
  (require 'org-roam-dailies)
  (add-hook 'org-roam-capture-new-node-hook #'org-node--add-series-item 90)
  (setq org-node-proposed-series-key series-key)
  (unwind-protect
      (org-roam-dailies--capture
       (encode-time
        (parse-time-string (concat ymd (format-time-string " %T %z"))))
       goto keys)
    (remove-hook 'org-roam-capture-new-node-hook #'org-node--add-series-item)
    (setq org-node-proposed-series-key nil)))


;;;; Obsolete

(org-node-changes--def-whiny-alias 'org-node-new-via-roam-capture
                                   'org-node-fakeroam-new-via-roam-capture
                                   "2024-09-17" nil "2024 November 30")

(org-node-changes--def-whiny-alias 'org-node-slugify-like-roam-actual
                                   'org-node-fakeroam-slugify-via-roam
                                   "2024-09-17" nil "2024 November 30")

(org-node-changes--def-whiny-alias 'org-node-fakeroam--db-update-files
                                   'org-node-fakeroam--update-db)

(org-node-changes--def-whiny-alias 'org-node-fakeroam-fast-render-clean-cache
                                   'org-node-fakeroam--clean-stale-previews
                                   nil t)

(define-obsolete-function-alias
  'org-node-fakeroam-show-roam-buffer
  'org-node-fakeroam-show-buffer
  "2024-10-19")

(defun org-node-fakeroam-setup-persistence ()
  "Set `org-node-fakeroam-persist-previews' to t.

Will be removed eventually.  Configure that variable instead."
  (declare (obsolete nil "2024-10-19"))
  (setq org-node-fakeroam-persist-previews t))
(defalias 'org-node-fakeroam-setup-persist #'org-node-fakeroam-setup-persistence)
(defalias 'org-node-fakeroam-enable-persist #'org-node-fakeroam-setup-persistence)

(provide 'org-node-fakeroam)

;;; org-node-fakeroam.el ends here
