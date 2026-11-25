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
              ;; The message no longer toggles: its actions are in its
              ;; header line.
              (goto-char (point-min))
              (search-forward "look")
              (expect (get-text-property (match-beginning 0) 'keymap)
                      :to-be nil))
          (kill-buffer buf))))

    (it "toggles the thumbnail on RET, nothing elsewhere in the message"
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
              (expect 'eca-chat--expandable-content-toggle :not :to-have-been-called))
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

;;; Inline images sent by the assistant

(defconst eca-chat-image-test--native-size '(1000 . 2000)
  "Size in pixels of the fake image of inline image specs.")

(defun eca-chat-image-test--fake-create-image (data type _data-p &rest props)
  "Return the spec of an image of DATA and TYPE with PROPS.
Like `create-image', which batch Emacs can't run."
  `(image :type ,type :data ,data ,@props))

(defun eca-chat-image-test--fake-image-size (spec &rest _)
  "Return the size of the fake image of SPEC, as Emacs shows it.
That is scaled down to fit its max width and height."
  (let* ((w (float (car eca-chat-image-test--native-size)))
         (h (float (cdr eca-chat-image-test--native-size)))
         (max-w (plist-get (cdr spec) :max-width))
         (max-h (plist-get (cdr spec) :max-height))
         (scale (min 1.0
                     (if max-w (/ max-w w) 1.0)
                     (if max-h (/ max-h h) 1.0))))
    (cons (round (* w scale)) (round (* h scale)))))

(defmacro eca-chat-image-test--with-chat (&rest body)
  "Run BODY in a new buffer laid out like a chat, then kill it."
  (declare (indent 0) (debug t))
  (let ((buf (make-symbol "buf")))
    `(let ((,buf (eca-chat-image-test--make-chat-buffer)))
       (unwind-protect (with-current-buffer ,buf ,@body)
         (kill-buffer ,buf)))))

(defun eca-chat-image-test--image-content (&optional media-type)
  "Return the content of an image of MEDIA-TYPE sent by the assistant."
  (list :type "image" :mediaType (or media-type "image/png")
        :base64 (base64-encode-string "fake-png-bytes")))

(defun eca-chat-image-test--render-image ()
  "Render an image sent by the assistant, return its overlay."
  (eca-chat--render-image-content (eca-chat-image-test--image-content) nil)
  (eca-chat--last-image-overlay))

(defun eca-chat-image-test--max-height (ov)
  "Return the max height of the image shown by OV."
  (image-property (overlay-get ov 'display) :max-height))

(defun eca-chat-image-test--shown-height (ov)
  "Return the height in pixels the image of OV shows at."
  (cdr (image-size (overlay-get ov 'display) t)))

(defun eca-chat-image-test--header (ov)
  "Return the text of the header line above the image of OV."
  (let ((bounds (eca-chat--image-header-bounds ov)))
    (buffer-substring-no-properties (car bounds) (cdr bounds))))

(defun eca-chat-image-test--goto-action (ov label)
  "Move point to the LABEL action in the header of the image of OV."
  (let ((bounds (eca-chat--image-header-bounds ov)))
    (goto-char (car bounds))
    (search-forward label (cdr bounds))
    (goto-char (match-beginning 0))))

(defun eca-chat-image-test--ret-on-image (ov)
  "Press RET on the image of OV, run by the keymap of the image."
  (goto-char (overlay-start ov))
  (funcall (lookup-key (overlay-get ov 'keymap) (kbd "RET"))))

(describe "inline images"
  (before-each
    (spy-on 'eca-session :and-return-value (make-eca--session))
    (spy-on 'display-graphic-p :and-return-value t)
    (spy-on 'display-images-p :and-return-value t)
    (spy-on 'image-type-available-p :and-return-value t)
    (spy-on 'create-image :and-call-fake #'eca-chat-image-test--fake-create-image)
    (spy-on 'image-size :and-call-fake #'eca-chat-image-test--fake-image-size)
    ;; 600 pixels wide at most: the fake image shows 600x1200 expanded.
    (spy-on 'eca-chat--resolve-image-max-width :and-return-value 600)
    (spy-on 'eca-info)
    (spy-on 'eca-warn))

  (describe "rendering"
    (it "starts compact, under a header line of actions on it"
      (eca-chat-image-test--with-chat
        (let ((ov (eca-chat-image-test--render-image)))
          (expect (eca-chat-image-test--max-height ov)
                  :to-equal eca-chat-image-compact-height)
          (expect (eca-chat-image-test--shown-height ov) :to-equal 300)
          (expect (image-property (overlay-get ov 'display) :max-width)
                  :to-equal 600)
          (expect (buffer-substring-no-properties (overlay-start ov)
                                                  (overlay-end ov))
                  :to-equal " ")
          (expect (eca-chat-image-test--header ov)
                  :to-equal "Image · PNG · 14 B    −   +    expand    save ")
          ;; The header is the line right above the image.
          (goto-char (overlay-start ov))
          (forward-line -1)
          (expect (point) :to-equal (car (eca-chat--image-header-bounds ov)))
          (expect (get-text-property (point) 'eca-no-fontify) :to-be t))))

    (it "starts expanded with eca-chat-image-start-expanded"
      (let ((eca-chat-image-start-expanded t))
        (eca-chat-image-test--with-chat
          (let ((ov (eca-chat-image-test--render-image)))
            (expect (eca-chat-image-test--max-height ov) :to-be nil)
            (expect (eca-chat-image-test--shown-height ov) :to-equal 1200)
            (expect (eca-chat-image-test--header ov) :to-match "shrink")))))

    (it "draws the actions as buttons telling what they do"
      (eca-chat-image-test--with-chat
        (let ((ov (eca-chat-image-test--render-image)))
          (eca-chat-image-test--goto-action ov "save")
          (expect (get-text-property (point) 'font-lock-face)
                  :to-be (eca--button-face 'eca-chat-image-action-face))
          (expect (get-text-property (point) 'help-echo)
                  :to-equal "Save the image to a file")
          (expect (get-text-property (point) 'mouse-face) :to-be 'highlight)
          (expect (funcall (overlay-get ov 'help-echo) nil ov nil)
                  :to-equal "RET or mouse-2: expand, +/-: zoom, s: save"))))

    (it "caps both sizes with eca-chat-image-max-height"
      (expect (eca-chat--image-max-height 300) :to-equal 300)
      (expect (eca-chat--image-max-height nil) :to-be nil)
      (let ((eca-chat-image-max-height 200))
        (expect (eca-chat--image-max-height 300) :to-equal 200)
        (expect (eca-chat--image-max-height 100) :to-equal 100)
        (expect (eca-chat--image-max-height nil) :to-equal 200)))

    (it "falls back to text in frames without images"
      (spy-on 'display-graphic-p :and-return-value nil)
      (eca-chat-image-test--with-chat
        (eca-chat-image-test--render-image)
        (expect (eca-chat--last-image-overlay) :to-be nil)
        (expect (buffer-string) :to-match "\\[Image: image/png, 14 bytes\\]")
        (expect (text-property-any (point-min) (point-max)
                                   'eca-chat-image-header t)
                :to-be nil)))

    (it "falls back to text inside subagent blocks"
      (spy-on 'eca-chat--update-expandable-content)
      (eca-chat-image-test--with-chat
        (eca-chat--render-image-content (eca-chat-image-test--image-content)
                                        "tool-1")
        (expect (eca-chat--last-image-overlay) :to-be nil)
        (expect (car (spy-calls-args-for 'eca-chat--update-expandable-content 0))
                :to-equal "tool-1")
        (expect (nth 2 (spy-calls-args-for 'eca-chat--update-expandable-content 0))
                :to-match "\\[Image: image/png, 14 bytes\\]")))

    (it "goes away with its text, e.g. on a rollback"
      (eca-chat-image-test--with-chat
        (let* ((ov (eca-chat-image-test--render-image))
               (start (car (eca-chat--image-header-bounds ov))))
          (let ((inhibit-read-only t))
            (delete-region start (overlay-end ov)))
          (expect (overlay-buffer ov) :to-be nil)
          (expect (eca-chat--last-image-overlay) :to-be nil)))))

  (describe "expanding"
    (it "expands and shrinks back with RET on the image"
      (eca-chat-image-test--with-chat
        (let ((ov (eca-chat-image-test--render-image)))
          (eca-chat-image-test--ret-on-image ov)
          (expect (eca-chat-image-test--max-height ov) :to-be nil)
          (expect (eca-chat-image-test--shown-height ov) :to-equal 1200)
          (expect (eca-chat-image-test--header ov) :to-match "shrink")
          (expect (funcall (overlay-get ov 'help-echo) nil ov nil)
                  :to-match "RET or mouse-2: shrink")
          (eca-chat-image-test--ret-on-image ov)
          (expect (eca-chat-image-test--max-height ov) :to-equal 300)
          (expect (eca-chat-image-test--header ov) :to-match "expand")
          ;; A middle click does the same.
          (expect (lookup-key (overlay-get ov 'keymap) [mouse-2])
                  :to-be (lookup-key (overlay-get ov 'keymap) (kbd "RET"))))))

    (it "expands and shrinks back with its button, point staying on it"
      (eca-chat-image-test--with-chat
        (let ((ov (eca-chat-image-test--render-image)))
          (eca-chat-image-test--goto-action ov "expand")
          (let ((pos (point)))
            (eca-chat--key-pressed-return)
            (expect (eca-chat-image-test--max-height ov) :to-be nil)
            (expect (point) :to-equal pos)
            (expect (looking-at-p "shrink") :to-be-truthy)
            (eca-chat--key-pressed-return)
            (expect (eca-chat-image-test--max-height ov) :to-equal 300)
            (expect (point) :to-equal pos)
            (expect (looking-at-p "expand") :to-be-truthy)))))

    (it "keeps the point after the button where it was"
      (eca-chat-image-test--with-chat
        (let ((ov (eca-chat-image-test--render-image)))
          (eca-chat-image-test--goto-action ov "save")
          (let ((pos (point)))
            (eca-chat-image-toggle-size ov)
            (expect (point) :to-equal pos)
            (expect (looking-at-p "save") :to-be-truthy)))))

    (it "keeps the header read-only when its button changes"
      (eca-chat-image-test--with-chat
        (let ((ov (eca-chat-image-test--render-image)))
          (eca-chat--protect-non-prompt)
          (eca-chat-image-toggle-size ov)
          (eca-chat-image-test--goto-action ov "shrink")
          (expect (get-text-property (point) 'read-only) :to-be t)
          (expect (get-text-property (point) 'eca-chat-image-header) :to-be t)
          (expect (insert "x") :to-throw 'text-read-only))))

    (it "toggles with RET right after the image"
      (eca-chat-image-test--with-chat
        (let ((ov (eca-chat-image-test--render-image)))
          (goto-char (overlay-end ov))
          (eca-chat--key-pressed-return)
          (expect (eca-chat-image-test--max-height ov) :to-be nil)))))

  (describe "zooming"
    (it "zooms in and out with the + and − buttons"
      (eca-chat-image-test--with-chat
        (let ((ov (eca-chat-image-test--render-image)))
          (eca-chat-image-test--goto-action ov "+")
          (eca-chat--key-pressed-return)
          (expect (eca-chat-image-test--max-height ov) :to-equal 360)
          (expect 'eca-info :to-have-been-called-with "Image zoom: %d%%" 30)
          (expect (eca-chat-image-test--header ov) :to-match "expand")
          (eca-chat-image-test--goto-action ov "−")
          (eca-chat--key-pressed-return)
          (expect (eca-chat-image-test--max-height ov) :to-equal 300)
          (expect 'eca-info :to-have-been-called-with "Image zoom: %d%%" 25))))

    (it "expands an image zoomed in up to its full size"
      (let ((eca-chat-image-scale-step 2))
        (eca-chat-image-test--with-chat
          (let ((ov (eca-chat-image-test--render-image)))
            (eca-chat-image-zoom-in ov)
            (expect (eca-chat-image-test--max-height ov) :to-equal 600)
            (eca-chat-image-zoom-in ov)
            (expect (eca-chat-image-test--max-height ov) :to-be nil)
            (expect (eca-chat-image-test--header ov) :to-match "shrink")
            (expect 'eca-info :to-have-been-called-with "Image zoom: %d%%" 100)
            (eca-chat-image-zoom-in ov)
            (expect 'eca-info :to-have-been-called-with
                    "Image already at its full size")))))

    (it "zooms out an expanded image from its full size"
      (eca-chat-image-test--with-chat
        (let ((ov (eca-chat-image-test--render-image)))
          (eca-chat-image-toggle-size ov)
          (eca-chat-image-zoom-out ov)
          (expect (eca-chat-image-test--max-height ov) :to-equal 1000)
          (expect (eca-chat-image-test--header ov) :to-match "expand"))))

    (it "stops shrinking an image at a floor"
      (let ((eca-chat-image-scale-step 2))
        (eca-chat-image-test--with-chat
          (let ((ov (eca-chat-image-test--render-image)))
            (dotimes (_ 3) (eca-chat-image-zoom-out ov))
            (expect (eca-chat-image-test--max-height ov)
                    :to-equal eca-chat--image-min-height)
            (eca-chat-image-zoom-out ov)
            (expect (eca-chat-image-test--max-height ov)
                    :to-equal eca-chat--image-min-height)
            (expect 'eca-info :to-have-been-called-with
                    "Image already at its smallest size")))))

    (it "resets the size of an image to the one it started at"
      (eca-chat-image-test--with-chat
        (let ((ov (eca-chat-image-test--render-image)))
          (eca-chat-image-zoom-in ov)
          (eca-chat-image-zoom-reset ov)
          (expect (eca-chat-image-test--max-height ov) :to-equal 300)
          (let ((eca-chat-image-start-expanded t))
            (eca-chat-image-zoom-reset ov))
          (expect (eca-chat-image-test--max-height ov) :to-be nil))))

    (it "zooms the image at point with its keys"
      (eca-chat-image-test--with-chat
        (let* ((ov (eca-chat-image-test--render-image))
               (map (overlay-get ov 'keymap)))
          (goto-char (overlay-start ov))
          (funcall (lookup-key map "+"))
          (expect (eca-chat-image-test--max-height ov) :to-equal 360)
          (funcall (lookup-key map "-"))
          (expect (eca-chat-image-test--max-height ov) :to-equal 300)
          (expect (lookup-key map "=") :to-be #'eca-chat-image-zoom-in)
          (expect (lookup-key map "0") :to-be #'eca-chat-image-zoom-reset)
          (expect (lookup-key map "s") :to-be #'eca-chat-save-image-at-point)))))

  (describe "commands"
    (it "act on the image at point, below its header line or else the last"
      (eca-chat-image-test--with-chat
        (let ((first (eca-chat-image-test--render-image))
              (second (eca-chat-image-test--render-image)))
          (goto-char (overlay-start first))
          (eca-chat-image-toggle-size)
          (expect (eca-chat-image-test--max-height first) :to-be nil)
          (expect (eca-chat-image-test--max-height second) :to-equal 300)
          ;; The header line of an image, off its buttons.
          (goto-char (car (eca-chat--image-header-bounds second)))
          (eca-chat-image-zoom-in)
          (expect (eca-chat-image-test--max-height second) :to-equal 360)
          ;; The prompt: the last image of the chat.
          (goto-char (point-max))
          (eca-chat-image-zoom-reset)
          (expect (eca-chat-image-test--max-height second) :to-equal 300)
          (expect (eca-chat-image-test--max-height first) :to-be nil))))

    (it "warn when the chat has no image"
      (eca-chat-image-test--with-chat
        (eca-chat-image-toggle-size)
        (eca-chat-image-zoom-in)
        (expect 'eca-warn :to-have-been-called-with
                "No image at point or in this chat")
        (expect 'eca-info :not :to-have-been-called)))

    (it "are bound under C-c C-z in chats"
      (expect (lookup-key eca-chat-mode-map (kbd "C-c C-z t"))
              :to-be #'eca-chat-image-toggle-size)
      (expect (lookup-key eca-chat-mode-map (kbd "C-c C-z +"))
              :to-be #'eca-chat-image-zoom-in)
      (expect (lookup-key eca-chat-mode-map (kbd "C-c C-z -"))
              :to-be #'eca-chat-image-zoom-out)
      (expect (lookup-key eca-chat-mode-map (kbd "C-c C-z 0"))
              :to-be #'eca-chat-image-zoom-reset)
      (expect (lookup-key eca-chat-mode-map (kbd "C-c C-z s"))
              :to-be #'eca-chat-save-image-at-point))

    (it "save the image of the save button"
      (spy-on 'eca-chat-save-image-at-point)
      (eca-chat-image-test--with-chat
        (let ((ov (eca-chat-image-test--render-image)))
          (eca-chat-image-test--render-image)
          (eca-chat-image-test--goto-action ov "save")
          (eca-chat--key-pressed-return)
          (expect 'eca-chat-save-image-at-point :to-have-been-called-with ov))))))

(describe "eca-chat--resolve-image-max-width"
  (it "returns the configured width, unscaled"
    (let ((eca-chat-image-max-width 640))
      (expect (eca-chat--resolve-image-max-width) :to-equal 640))
    (let ((eca-chat-image-max-width nil))
      (expect (eca-chat--resolve-image-max-width) :to-be nil)))

  (it "fits the frame of hidden chats, up to eca-chat-image-window-fit-cap"
    (let ((eca-chat-image-max-width 'fit-window)
          (eca-chat-image-window-fit-cap 800))
      ;; Not shown in any window.
      (with-temp-buffer
        (spy-on 'frame-pixel-width :and-return-value 1500)
        (expect (eca-chat--resolve-image-max-width) :to-equal 800)
        (spy-on 'frame-pixel-width :and-return-value 500)
        (expect (eca-chat--resolve-image-max-width) :to-equal 500)))))

(provide 'eca-chat-image-test)
;;; eca-chat-image-test.el ends here
