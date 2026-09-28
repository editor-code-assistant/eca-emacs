;;; eca-chat-image-test.el --- Tests for eca-chat-image -*- lexical-binding: t; -*-
;;; Commentary:
;; Tests for image mention thumbnails: image chips of the prompt and
;; context line, and image mentions of sent messages.
;;; Code:
(require 'buttercup)
(require 'eca-chat)

(defvar eca-chat-image-test--files nil
  "Temporary image files created by the running spec.")

;; Kept out of `expect' args: on Emacs 29 buttercup's interpreted
;; oclosure thunks shadow the `:type' keyword inside them.
(defconst eca-chat-image-test--thumbnail '(image :type png :file "thumb")
  "Fake thumbnail returned by the stubbed `create-image'.")

(defun eca-chat-image-test--image-file ()
  "Create an empty temporary png file, deleted after the spec."
  (car (push (make-temp-file "eca-image-test-" nil ".png")
             eca-chat-image-test--files)))

(defun eca-chat-image-test--allow-display ()
  "Pretend the frame displays images, with a fake thumbnail."
  (spy-on 'display-images-p :and-return-value t)
  (spy-on 'create-image :and-return-value eca-chat-image-test--thumbnail))

(defun eca-chat-image-test--make-chat-buffer ()
  "Create a buffer laid out like a chat, with an empty prompt.
Caller must kill it."
  (let ((buf (generate-new-buffer " *test-chat-image*")))
    (with-current-buffer buf
      (insert "\n")
      (eca-chat--insert-prompt-string)
      (setq major-mode 'eca-chat-mode)
      (setq-local eca-chat--id "chat-1")
      (setq-local eca-chat-expandable--id->ov (make-hash-table :test 'equal)))
    buf))

(defun eca-chat-image-test--insert-chip (path)
  "Insert the prompt chip of the image file context of PATH.
Insert a space after it, like pasting does, and return the chip
start."
  (let ((start (point)))
    (insert (eca-chat--context->str (list :type "file" :path path) 'static) " ")
    start))

(defun eca-chat-image-test--watch-insertions ()
  "Show the thumbnails of image mentions inserted in this buffer.
Like `eca-chat-mode' and `eca-chat-compose-mode' do."
  (add-hook 'after-change-functions
            #'eca-chat--auto-show-image-thumbnails-after-change nil t))

(defun eca-chat-image-test--thumbnails ()
  "Return the thumbnail overlays of the current buffer."
  (seq-filter (lambda (ov) (overlay-get ov 'eca-chat-image-thumbnail))
              (overlays-in (point-min) (point-max))))

(defun eca-chat-image-test--render-user-message (session buf text)
  "Render TEXT as a user message of SESSION into BUF."
  (spy-on 'font-lock-ensure)
  (spy-on 'eca-chat--ensure-prompt-visible)
  (eca-chat--render-content session buf "user"
                            (list :type "text" :text text :contentId "u1")
                            (eca--session-workspace-folders session)))

(describe "image mention thumbnails"
  (before-each
    (spy-on 'eca-session :and-return-value (make-eca--session)))

  (after-each
    (dolist (file eca-chat-image-test--files)
      (ignore-errors (delete-file file)))
    (setq eca-chat-image-test--files nil))

  (describe "image chips"
    (it "marks the chips of image files with their path"
      (let ((str (eca-chat--context->str
                  (list :type "file" :path "/tmp/shot.PNG") 'static)))
        (expect (get-text-property 0 'eca-chat-image-path str)
                :to-equal "/tmp/shot.PNG")
        (expect (get-text-property (1- (length str)) 'eca-chat-image-path str)
                :to-equal "/tmp/shot.PNG")))

    (it "leaves the chips of other files and directories alone"
      (let ((file (eca-chat--context->str
                   (list :type "file" :path "/tmp/foo.el") 'static))
            (dir (eca-chat--context->str
                  (list :type "directory" :path "/tmp/pics.png") 'static)))
        (expect (get-text-property 0 'eca-chat-image-path file) :to-be nil)
        (expect (get-text-property 0 'eca-chat-image-path dir) :to-be nil)))

    (it "marks #filepath mentions of image files"
      (expect (get-text-property 0 'eca-chat-image-path
                                 (eca-chat--filepath->str "/tmp/a.jpg" nil))
              :to-equal "/tmp/a.jpg")))

  (describe "eca-chat--image-link-at"
    (it "returns the chip bounds and path from any position on it"
      (with-temp-buffer
        (insert "see ")
        (let* ((start (eca-chat-image-test--insert-chip "/tmp/shot.png"))
               (end (+ start (length "@/tmp/shot.png"))))
          (expect (eca-chat--image-link-at start)
                  :to-equal (list start end "/tmp/shot.png"))
          (expect (eca-chat--image-link-at (+ start 5))
                  :to-equal (list start end "/tmp/shot.png"))
          (expect (eca-chat--image-link-at end) :to-be nil)
          (expect (eca-chat--image-link-at (1- start)) :to-be nil)))))

  (describe "eca-chat-toggle-image-thumbnail"
    (it "shows the thumbnail in place of the chip, then hides it"
      (eca-chat-image-test--allow-display)
      (let ((file (eca-chat-image-test--image-file))
            (buf (eca-chat-image-test--make-chat-buffer)))
        (unwind-protect
            (with-current-buffer buf
              (goto-char (point-max))
              (let* ((start (eca-chat-image-test--insert-chip file))
                     (prompt (progn (insert "describe it")
                                    (eca-chat--prompt-content))))
                (goto-char start)
                (eca-chat-toggle-image-thumbnail)
                (let ((ov (car (eca-chat-image-test--thumbnails))))
                  (expect (overlay-start ov) :to-equal start)
                  (expect (overlay-end ov)
                          :to-equal (+ start 1 (length file)))
                  (expect (overlay-get ov 'display)
                          :to-be eca-chat-image-test--thumbnail))
                (expect 'create-image :to-have-been-called-with
                        file nil nil
                        :max-width eca-chat-image-thumbnail-size
                        :max-height eca-chat-image-thumbnail-size
                        :ascent 'center)
                (expect (eca-chat--prompt-content) :to-equal prompt)
                (eca-chat-toggle-image-thumbnail)
                (expect (eca-chat-image-test--thumbnails) :to-be nil)))
          (kill-buffer buf))))

    (it "warns without a thumbnail when the image file is gone"
      (eca-chat-image-test--allow-display)
      (spy-on 'eca-warn)
      (with-temp-buffer
        (goto-char (eca-chat-image-test--insert-chip "/nope/gone.png"))
        (eca-chat-toggle-image-thumbnail)
        (expect 'eca-warn :to-have-been-called-with
                "Image not found: %s" "/nope/gone.png")
        (expect (eca-chat-image-test--thumbnails) :to-be nil)))

    (it "warns without a thumbnail when the frame can't display images"
      (spy-on 'display-images-p :and-return-value nil)
      (spy-on 'eca-warn)
      (with-temp-buffer
        (goto-char (eca-chat-image-test--insert-chip
                    (eca-chat-image-test--image-file)))
        (eca-chat-toggle-image-thumbnail)
        (expect 'eca-warn :to-have-been-called)
        (expect (eca-chat-image-test--thumbnails) :to-be nil)))

    (it "goes away with the chip text"
      (eca-chat-image-test--allow-display)
      (with-temp-buffer
        (let ((start (eca-chat-image-test--insert-chip
                      (eca-chat-image-test--image-file))))
          (goto-char start)
          (eca-chat-toggle-image-thumbnail)
          (expect (length (eca-chat-image-test--thumbnails)) :to-equal 1)
          (delete-region start (cadr (eca-chat--image-link-at start)))
          (expect (eca-chat-image-test--thumbnails) :to-be nil))))

    (it "keeps text typed around the chip out of the thumbnail"
      (eca-chat-image-test--allow-display)
      (with-temp-buffer
        (let* ((start (eca-chat-image-test--insert-chip
                       (eca-chat-image-test--image-file)))
               (end (cadr (eca-chat--image-link-at start))))
          (goto-char start)
          (eca-chat-toggle-image-thumbnail)
          (goto-char end)
          (insert "after")
          (goto-char start)
          (insert "before")
          (let ((ov (car (eca-chat-image-test--thumbnails))))
            (expect (overlay-start ov) :to-equal (+ start (length "before")))
            (expect (overlay-end ov) :to-equal (+ end (length "before"))))))))

  (describe "thumbnails shown by default"
    (it "shows the thumbnail of an inserted image mention"
      (eca-chat-image-test--allow-display)
      (with-temp-buffer
        (eca-chat-image-test--watch-insertions)
        (let ((start (eca-chat-image-test--insert-chip
                      (eca-chat-image-test--image-file)))
              (ovs (eca-chat-image-test--thumbnails)))
          (expect (length ovs) :to-equal 1)
          (expect (overlay-start (car ovs)) :to-equal start))))

    (it "shows the thumbnails of chips recalled from the prompt history"
      (eca-chat-image-test--allow-display)
      (let ((file (eca-chat-image-test--image-file))
            (buf (eca-chat-image-test--make-chat-buffer)))
        (unwind-protect
            (with-current-buffer buf
              (eca-chat-image-test--watch-insertions)
              (eca-chat--set-prompt
               (concat "see " (eca-chat--context->str
                               (list :type "file" :path file) 'static)))
              (expect (length (eca-chat-image-test--thumbnails)) :to-equal 1))
          (kill-buffer buf))))

    (it "keeps inserted mentions as text when turned off"
      (eca-chat-image-test--allow-display)
      (let ((eca-chat-image-show-thumbnails nil))
        (with-temp-buffer
          (eca-chat-image-test--watch-insertions)
          (eca-chat-image-test--insert-chip (eca-chat-image-test--image-file))
          (expect (eca-chat-image-test--thumbnails) :to-be nil))))

    (it "silently keeps mentions of missing or remote files as text"
      (eca-chat-image-test--allow-display)
      (spy-on 'eca-warn)
      ;; Loading TRAMP checks files itself, so load it upfront.
      (file-remote-p "/ssh:host:/proj/a.png")
      (with-temp-buffer
        (eca-chat-image-test--watch-insertions)
        (eca-chat-image-test--insert-chip "/nope/gone.png")
        (eca-chat-image-test--insert-chip "/ssh:host:/proj/a.png")
        (expect (eca-chat-image-test--thumbnails) :to-be nil)
        (expect 'create-image :not :to-have-been-called)
        (expect 'eca-warn :not :to-have-been-called)))

    (it "silently keeps mentions as text in frames without images"
      (spy-on 'display-images-p :and-return-value nil)
      (spy-on 'eca-warn)
      (with-temp-buffer
        (eca-chat-image-test--watch-insertions)
        (eca-chat-image-test--insert-chip (eca-chat-image-test--image-file))
        (expect (eca-chat-image-test--thumbnails) :to-be nil)
        (expect 'eca-warn :not :to-have-been-called))))

  (describe "pasting an image"
    (before-each
      (spy-on 'eca-chat--select-window)
      (spy-on 'eca-info))

    (it "shows the thumbnail of the pasted image right away"
      (eca-chat-image-test--allow-display)
      (let ((file (eca-chat-image-test--image-file))
            (buf (eca-chat-image-test--make-chat-buffer)))
        (spy-on 'eca-chat-media--save-clipboard-image :and-return-value file)
        (spy-on 'eca-chat--get-last-buffer :and-return-value buf)
        (unwind-protect
            (with-current-buffer buf
              (eca-chat-image-test--watch-insertions)
              (goto-char (point-max))
              (eca-chat--yank-image-handler "image/png" "data")
              (expect (length (eca-chat-image-test--thumbnails)) :to-equal 1)
              (expect (point) :to-equal (point-max)))
          (kill-buffer buf))))

    (it "separates the chip from the word before it, point after it"
      (let ((file (eca-chat-image-test--image-file))
            (buf (eca-chat-image-test--make-chat-buffer)))
        (spy-on 'eca-chat-media--save-clipboard-image :and-return-value file)
        (spy-on 'eca-chat--get-last-buffer :and-return-value buf)
        (unwind-protect
            (with-current-buffer buf
              (goto-char (point-max))
              (insert "see the image")
              (eca-chat--yank-image-handler "image/png" "data")
              (expect (buffer-substring-no-properties
                       (eca-chat--prompt-field-start-point) (point-max))
                      :to-equal (concat "see the image @" file " "))
              (expect (point) :to-equal (point-max)))
          (kill-buffer buf))))

    (it "leaves point after the chip space in an empty prompt"
      (let ((file (eca-chat-image-test--image-file))
            (buf (eca-chat-image-test--make-chat-buffer)))
        (spy-on 'eca-chat-media--save-clipboard-image :and-return-value file)
        (spy-on 'eca-chat--get-last-buffer :and-return-value buf)
        (unwind-protect
            (with-current-buffer buf
              (goto-char (point-max))
              (eca-chat--yank-image-handler "image/png" "data")
              (expect (point) :to-equal (point-max))
              (expect (char-before) :to-equal ?\s)
              (expect (eca-chat--image-link-at (- (point) 2)) :not :to-be nil))
          (kill-buffer buf)))))

  (describe "RET in the prompt"
    (it "toggles the thumbnail with point on the chip instead of sending"
      (eca-chat-image-test--allow-display)
      (spy-on 'eca-chat--send-prompt)
      (let ((buf (eca-chat-image-test--make-chat-buffer)))
        (unwind-protect
            (with-current-buffer buf
              (goto-char (point-max))
              (let ((start (eca-chat-image-test--insert-chip
                            (eca-chat-image-test--image-file))))
                (goto-char start)
                (eca-chat--key-pressed-return)
                (expect (length (eca-chat-image-test--thumbnails)) :to-equal 1)
                (eca-chat--key-pressed-return)
                (expect (eca-chat-image-test--thumbnails) :to-be nil)
                (expect 'eca-chat--send-prompt :not :to-have-been-called)))
          (kill-buffer buf))))

    (it "sends the prompt with point right after the chip"
      (eca-chat-image-test--allow-display)
      (spy-on 'eca-chat--send-prompt)
      (let ((buf (eca-chat-image-test--make-chat-buffer)))
        (unwind-protect
            (with-current-buffer buf
              (goto-char (point-max))
              (let ((start (eca-chat-image-test--insert-chip
                            (eca-chat-image-test--image-file))))
                (goto-char (cadr (eca-chat--image-link-at start)))
                (eca-chat--key-pressed-return)
                (expect 'eca-chat--send-prompt :to-have-been-called)
                (expect (eca-chat-image-test--thumbnails) :to-be nil)))
          (kill-buffer buf))))

    (it "hides a shown thumbnail from its own keymap"
      (eca-chat-image-test--allow-display)
      (with-temp-buffer
        (goto-char (eca-chat-image-test--insert-chip
                    (eca-chat-image-test--image-file)))
        (eca-chat-toggle-image-thumbnail)
        (let ((ov (car (eca-chat-image-test--thumbnails))))
          (funcall (lookup-key (overlay-get ov 'keymap) (kbd "RET")))
          (expect (eca-chat-image-test--thumbnails) :to-be nil)))))

  (describe "context line"
    (it "shows new chips and keeps each chip state across refreshes"
      (eca-chat-image-test--allow-display)
      (let* ((buf (eca-chat-image-test--make-chat-buffer))
             (context (list :type "file" :path (eca-chat-image-test--image-file))))
        (unwind-protect
            (with-current-buffer buf
              (setq-local eca-chat--context (list context))
              (eca-chat--refresh-context)
              (let ((start (overlay-start (eca-chat--prompt-context-field-ov))))
                (expect (length (eca-chat-image-test--thumbnails)) :to-equal 1)
                ;; Hidden with RET, it stays hidden across refreshes.
                (goto-char start)
                (eca-chat-toggle-image-thumbnail)
                (eca-chat--refresh-context)
                (expect (eca-chat-image-test--thumbnails) :to-be nil)
                ;; Shown back, it stays shown.
                (goto-char start)
                (eca-chat-toggle-image-thumbnail)
                (eca-chat--refresh-context)
                (let ((ovs (eca-chat-image-test--thumbnails)))
                  (expect (length ovs) :to-equal 1)
                  (expect (overlay-start (car ovs)) :to-equal start)
                  (expect (get-text-property start 'eca-chat-context-item)
                          :to-equal context))
                (eca-chat--remove-context context)
                (expect (eca-chat-image-test--thumbnails) :to-be nil)))
          (kill-buffer buf)))))

  (describe "sent messages"
    (it "links the mentions of existing images only"
      (let* ((file (eca-chat-image-test--image-file))
             (buf (eca-chat-image-test--make-chat-buffer))
             (text (format "look at @%s, not @/nope/gone.png" file)))
        (unwind-protect
            (with-current-buffer buf
              (eca-chat-image-test--render-user-message (make-eca--session) buf text)
              (goto-char (point-min))
              (search-forward (concat "@" file))
              (let ((start (match-beginning 0))
                    (end (match-end 0)))
                (expect (eca-chat--image-link-at start)
                        :to-equal (list start end file))
                ;; The label keymap would catch RET before the chat one.
                (expect (get-text-property start 'keymap) :to-be nil)
                (expect (get-text-property end 'eca-chat-image-path) :to-be nil))
              (search-forward "@/nope/gone.png")
              (expect (get-text-property (match-beginning 0) 'eca-chat-image-path)
                      :to-be nil)
              (goto-char (point-min))
              (search-forward "look")
              (expect (get-text-property (match-beginning 0) 'keymap)
                      :to-be-truthy))
          (kill-buffer buf))))

    (it "toggles the thumbnail on RET, the rollback block elsewhere"
      (eca-chat-image-test--allow-display)
      (spy-on 'eca-chat--expandable-content-toggle)
      (let* ((file (eca-chat-image-test--image-file))
             (buf (eca-chat-image-test--make-chat-buffer)))
        (unwind-protect
            (with-current-buffer buf
              (eca-chat-image-test--render-user-message
               (make-eca--session) buf (format "look at @%s" file))
              ;; Shown right away; RET hides it and shows it back.
              (expect (length (eca-chat-image-test--thumbnails)) :to-equal 1)
              (goto-char (point-min))
              (search-forward (concat "@" file))
              (goto-char (match-beginning 0))
              (eca-chat--key-pressed-return)
              (expect (eca-chat-image-test--thumbnails) :to-be nil)
              (eca-chat--key-pressed-return)
              (expect (length (eca-chat-image-test--thumbnails)) :to-equal 1)
              (expect 'eca-chat--expandable-content-toggle :not :to-have-been-called)
              (goto-char (point-min))
              (search-forward "look")
              (eca-chat--key-pressed-return)
              (expect 'eca-chat--expandable-content-toggle
                      :to-have-been-called-with "u1"))
          (kill-buffer buf))))

    (it "maps server paths back to local ones"
      (let* ((file (eca-chat-image-test--image-file))
             (eca-local-to-remote-prefix-map
              (list (cons (file-name-directory file) "/workspace")))
             (buf (eca-chat-image-test--make-chat-buffer)))
        (unwind-protect
            (with-current-buffer buf
              (eca-chat-image-test--render-user-message
               (make-eca--session) buf
               (concat "@/workspace/" (file-name-nondirectory file)))
              (goto-char (point-min))
              (search-forward "@/workspace/")
              (expect (get-text-property (match-beginning 0) 'eca-chat-image-path)
                      :to-equal file))
          (kill-buffer buf))))

    (it "resolves mentions relative to the workspace roots"
      (let* ((file (eca-chat-image-test--image-file))
             (session (make-eca--session
                       :workspace-folders (list (file-name-directory file))))
             (buf (eca-chat-image-test--make-chat-buffer)))
        (unwind-protect
            (with-current-buffer buf
              (eca-chat-image-test--render-user-message
               session buf (concat "@" (file-name-nondirectory file) " please"))
              (goto-char (point-min))
              (search-forward "@eca-image-test-")
              (expect (get-text-property (match-beginning 0) 'eca-chat-image-path)
                      :to-equal file))
          (kill-buffer buf))))

    (it "links remote images without checking them"
      ;; Loading TRAMP checks files itself, so load it before spying.
      (file-remote-p "/ssh:host:/proj/a.png")
      (spy-on 'eca--path-remote-to-local :and-return-value "/ssh:host:/proj/a.png")
      (spy-on 'file-exists-p)
      (expect (eca-chat--mention-image-path "/proj/a.png" nil)
              :to-equal "/ssh:host:/proj/a.png")
      (expect 'file-exists-p :not :to-have-been-called))))

(provide 'eca-chat-image-test)
;;; eca-chat-image-test.el ends here
