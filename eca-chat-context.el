;;; eca-chat-context.el --- ECA chat context and completion -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Context management, completion-at-point, cursor tracking, and
;;  media/image handling for ECA chat.
;;
;;; Code:

(require 'f)
(require 'eca-util)
(require 'eca-api)
(require 'eca-chat-expandable)

;; Forward declarations for eca-chat.el core
(defvar eca-chat-mode-map)
(defvar eca-chat--id)
(defvar eca-chat-window-width)
(declare-function eca-chat--insert "eca-chat")
(declare-function eca-chat--prompt-field-start-point "eca-chat")
(declare-function eca-chat--prompt-context-field-ov "eca-chat")
(declare-function eca-chat--point-at-new-context-p "eca-chat")
(declare-function eca-chat--point-at-prompt-field-p "eca-chat")
(declare-function eca-chat--prepare-prompt-for-paste "eca-chat")
(declare-function eca-chat--new-context-start-point "eca-chat")
(declare-function eca-chat--select-window "eca-chat")
(declare-function eca-chat--get-last-buffer "eca-chat")
(declare-function eca-chat--insert-prompt "eca-chat")
(declare-function eca-chat--relativize-filename-for-workspace-root "eca-chat")
(declare-function eca-chat--task-find-by-id "eca-chat")

;;;; Customization

(defcustom eca-chat-auto-add-repomap nil
  "Whether to auto include repoMap context when opening eca."
  :type 'boolean
  :group 'eca)

(defcustom eca-chat-auto-add-cursor t
  "Whether to auto track cursor opened files/position and add them to context."
  :type 'boolean
  :group 'eca)

(defcustom eca-chat-cursor-context-debounce 0.3
  "Seconds to debounce updates when tracking cursor to context."
  :type 'number
  :group 'eca)

(defcustom eca-chat-context-prefix "@"
  "The context prefix string used in eca chat buffer."
  :type 'string
  :group 'eca)

(defcustom eca-chat-filepath-prefix "#"
  "The filepath prefix string used in eca chat buffer."
  :type 'string
  :group 'eca)

(defcustom eca-chat-yank-image-context-location 'user
  "Where to paste images from clipboard."
  :type '(choice (const :tag "System context area" system)
                 (const :tag "user context area" user))
  :group 'eca)

;;;; Faces

(defface eca-chat-context-unlinked-face
  '((((background dark))  (:foreground "gold" :height 0.9))
    (((background light)) (:foreground "dark goldenrod" :height 0.9)))
  "Face for contexts to be added."
  :group 'eca)

(defface eca-chat-context-file-face
  '((((background dark))  (:foreground "coral" :underline t :height 0.9))
    (((background light)) (:foreground "firebrick" :underline t :height 0.9)))
  "Face for contexts of file type."
  :group 'eca)

(defface eca-chat-context-repo-map-face
  '((((background dark))  (:foreground "turquoise" :underline t :height 0.9))
    (((background light)) (:foreground "dark cyan" :underline t :height 0.9)))
  "Face for contexts of repoMap type."
  :group 'eca)

(defface eca-chat-context-mcp-resource-face
  '((((background dark))  (:foreground "lime green" :underline t :height 0.9))
    (((background light)) (:foreground "dark green" :underline t :height 0.9)))
  "Face for contexts of mcpResource type."
  :group 'eca)

(defface eca-chat-context-cursor-face
  '((((background dark))  (:foreground "gainsboro" :underline t :height 0.9))
    (((background light)) (:foreground "dim gray" :underline t :height 0.9)))
  "Face for contexts of cursor type."
  :group 'eca)

;;;; Variables

(defvar-local eca-chat--context-completion-cache (make-hash-table :test 'equal))
(defvar-local eca-chat--file-completion-cache (make-hash-table :test 'equal))
(defvar-local eca-chat--command-completion-cache (make-hash-table :test 'equal))
(defvar-local eca-chat--context '())
(defvar-local eca-chat--cursor-context nil)

;; Timer used to debounce post-command driven context updates
(defvar eca-chat--cursor-context-timer nil)

;;;; Constants

(defconst eca-chat--kind->symbol
  '(("file" . file)
    ("directory" . folder)
    ("repoMap" . module)
    ("cursor" . class)
    ("mcpPrompt" . function)
    ("mcpResource" . file)
    ("native" . variable)
    ("custom-prompt" . method)))

(defconst eca-chat-media--mime-extension-map
  '(("image/png" . "png")
    ("image/x-png" . "png")
    ("image/jpeg" . "jpg")
    ("image/jpg" . "jpg")
    ("image/gif" . "gif")
    ("image/webp" . "webp")
    ("image/heic" . "heic")
    ("image/heif" . "heif")
    ("image/svg+xml" . "svg"))
  "Mapping of mime types to screenshot file extensions.")

;;;; Context functions

(defun eca-chat--context-presentable-path (filename)
  "Return the presentable string for FILENAME."
  (or (when (-first (lambda (root) (f-ancestor-of? root filename))
                    (eca--session-workspace-folders (eca-session)))
        (f-filename filename))
      filename))

(defun eca-chat--context->str (context &optional static?)
  "Convert CONTEXT to a presentable str in buffer.
If STATIC? return strs with no dynamic values."
  (-let* (((&plist :type type) context)
          (context-str
           (pcase type
             ("file" (let ((path (plist-get context :path))
                           (lines-range (plist-get context :linesRange)))
                       (propertize (concat eca-chat-context-prefix
                                           (eca-chat--context-presentable-path path)
                                           (-when-let ((&plist :start start :end end) lines-range)
                                             (format "(%d-%d)" start end)))
                                   'eca-chat-expanded-item-str (concat eca-chat-context-prefix path
                                                                       (-when-let ((&plist :start start :end end) lines-range)
                                                                         (format ":L%d-L%d" start end)))
                                   'font-lock-face 'eca-chat-context-file-face)))
             ("directory" (propertize (concat eca-chat-context-prefix (eca-chat--context-presentable-path (plist-get context :path)))
                                      'eca-chat-expanded-item-str (concat eca-chat-context-prefix (plist-get context :path))
                                      'font-lock-face 'eca-chat-context-file-face))
             ("repoMap" (propertize (concat eca-chat-context-prefix "repoMap")
                                    'eca-chat-expanded-item-str (concat eca-chat-context-prefix "repoMap")
                                    'font-lock-face 'eca-chat-context-repo-map-face))
             ("mcpResource" (propertize (concat eca-chat-context-prefix (plist-get context :server) ":" (plist-get context :name))
                                        'eca-chat-expanded-item-str (concat eca-chat-context-prefix (plist-get context :server) ":" (plist-get context :name))
                                        'font-lock-face 'eca-chat-context-mcp-resource-face))
             ("cursor" (propertize (if static?
                                       (concat eca-chat-context-prefix "cursor")
                                     (concat eca-chat-context-prefix "cursor"
                                             "("
                                             (-some-> (plist-get eca-chat--cursor-context :path)
                                               (f-filename))
                                             " "
                                             (-some->>
                                                 (-> eca-chat--cursor-context
                                                     (plist-get :position)
                                                     (plist-get :start)
                                                     (plist-get :line))
                                               (funcall #'number-to-string))
                                             ":"
                                             (-some->>
                                                 (-> eca-chat--cursor-context
                                                     (plist-get :position)
                                                     (plist-get :start)
                                                     (plist-get :character))
                                               (funcall #'number-to-string))
                                             ")"))
                                   'eca-chat-expanded-item-str (concat eca-chat-context-prefix "cursor")
                                   'font-lock-face 'eca-chat-context-cursor-face))
             (_ (concat eca-chat-context-prefix "unknown:" type)))))
    (propertize context-str
                'eca-chat-item-type 'context
                'eca-chat-item-str-length (length context-str)
                'eca-chat-context-item context)))

(defun eca-chat--filepath->str (filepath lines-range)
  "Convert FILEPATH and LINES-RANGE to a presentable str in buffer."
  (let* ((item-str (concat eca-chat-filepath-prefix
                           (eca-chat--context-presentable-path filepath)
                           (-when-let ((&plist :start start :end end) lines-range)
                             (format "(%d-%d)" start end)))))
    (propertize item-str
                'eca-chat-item-type 'filepath
                'eca-chat-item-str-length (length item-str)
                'eca-chat-expanded-item-str (concat eca-chat-filepath-prefix
                                                    filepath
                                                    (-when-let ((&plist :start start :end end) lines-range)
                                                      (format ":L%d-L%d" start end)))
                'font-lock-face 'eca-chat-context-file-face)))

(defun eca-chat--refresh-context ()
  "Refresh chat context."
  (save-excursion
    (-some-> (eca-chat--prompt-context-field-ov)
      (overlay-start)
      (goto-char))
    (delete-region (point) (line-end-position))
    (seq-doseq (context eca-chat--context)
      (eca-chat--insert (eca-chat--context->str context))
      (eca-chat--insert " "))
    (eca-chat--insert (propertize eca-chat-context-prefix 'font-lock-face 'eca-chat-context-unlinked-face))))

(defun eca-chat--add-context (context)
  "Add to chat CONTEXT."
  (add-to-list 'eca-chat--context context t)
  (eca-chat--refresh-context))

(defun eca-chat--remove-context (context)
  "Remove from chat CONTEXT."
  (setq eca-chat--context (remove context eca-chat--context))
  (eca-chat--refresh-context))

;;;; Cursor tracking

(defun eca-chat--cur-position ()
  "Return the start and end positions for current point.
Resteps a cons cell (START . END) where START and END are cons cells
of (LINE . CHARACTER) representing the current selection or cursor position."
  (save-excursion
    (let* ((start-pos (if (use-region-p) (region-beginning) (point)))
           (end-pos (if (use-region-p) (region-end) (point)))
           (start-line (line-number-at-pos start-pos))
           (start-char (1+ (progn
                             (goto-char start-pos)
                             (current-column))))
           (end-line (line-number-at-pos end-pos))
           (end-char (1+ (progn
                           (goto-char end-pos)
                           (current-column)))))
      (cons (cons start-line start-char)
            (cons end-line end-char)))))

(defun eca-chat--get-last-visited-buffer ()
  "Return the last visited buffer which has a filename."
  (-first (lambda (buff)
            (when (buffer-live-p buff)
              (with-current-buffer buff
                (buffer-file-name))))
          (buffer-list)))

(defun eca-chat--track-cursor (&rest _args)
  "Change chat context considering current open file and point."
  (when-let ((session (eca-session)))
    (when-let ((workspaces (eca--session-workspace-folders session)))
      (when-let ((buffer (eca-chat--get-last-visited-buffer)))
        (when-let ((path (buffer-file-name buffer)))
          (when (--any? (and it (f-ancestor-of? it path))
                        workspaces)
            (with-current-buffer buffer
              (when-let (chat-buffer (eca-chat--get-last-buffer session))
                (when (buffer-live-p chat-buffer)
                  (-let* (((start . end) (eca-chat--cur-position))
                          ((start-line . start-character) start)
                          ((end-line . end-character) end))
                    (eca-chat--with-current-buffer chat-buffer
                      (let ((new-context (list :path path
                                               :position (list :start (list :line start-line :character start-character)
                                                               :end (list :line end-line :character end-character)))))
                        (when (not (eca-plist-equal eca-chat--cursor-context new-context))
                          (setq eca-chat--cursor-context new-context)
                          (eca-chat--refresh-context))))))))))))))

(defun eca-chat--track-cursor-position-schedule ()
  "Debounce `eca-chat--track-cursor' via an idle timer."
  (unless eca-chat--cursor-context-timer
    (setq eca-chat--cursor-context-timer
          (run-with-idle-timer eca-chat-cursor-context-debounce t
                               #'eca-chat--track-cursor))))

;;;; Media/yank functions

(defun eca-chat-media--extension-for-type (type)
  "Return file extension (without dot) for mime TYPE.
TYPE can be a string or symbol."
  (let* ((type-str (if (symbolp type) (symbol-name type) type))
         (clean (and type-str (string-trim type-str))))
    (or (cdr (assoc-string clean eca-chat-media--mime-extension-map t))
        (when clean
          (let* ((parts (split-string clean "/"))
                 (raw-subtype (cadr parts))
                 (subtype (car (split-string (or raw-subtype "") "\\+"))))
            (unless (string-empty-p subtype)
              subtype)))
        "png")))

(defun eca-chat--yank-image-handler (type data)
  "Handler for `yank-media' to insert images from clipboard.
TYPE is the MIME type (e.g., image/png).
DATA is the binary image data as a string."
  (when-let* ((session (eca-session))
              (chat-buffer (eca-chat--get-last-buffer session))
              (extension (eca-chat-media--extension-for-type type))
              (output-path (make-temp-file "eca-screenshot-" nil (concat "." extension))))
    (condition-case err
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (write-region data nil output-path nil 'silent))
          (when (f-exists? output-path)
            (eca-chat--with-current-buffer chat-buffer
              (let ((context (list :type "file" :path output-path))
                    (file-size (file-size-human-readable (file-attribute-size (file-attributes output-path)))))
                (eca-chat--select-window)
                (if (eq 'system eca-chat-yank-image-context-location)
                    (eca-chat--add-context context)
                  (progn
                    (eca-chat--insert-prompt (concat (eca-chat--context->str context 'static) " "))
                    (goto-char (+ (point) (+ 2 (length output-path))))))
                (eca-info "Image added, size: %s" file-size)))))
      (error
       (eca-error "Failed to save yanked image: %s" (error-message-string err))))))

(defun eca-chat--yank-considering-image (orig-fun &rest args)
  "Around advice for paste commands to use `yank-media' for images.
Call ORIG-FUN with ARGS if not media."
  (when (derived-mode-p 'eca-chat-mode)
    (eca-chat--prepare-prompt-for-paste))
  (if (and (display-graphic-p)
           (derived-mode-p 'eca-chat-mode)
           (fboundp 'yank-media)
           (boundp 'yank-media--registered-handlers)
           yank-media--registered-handlers
           (when-let* ((targets (gui-get-selection 'CLIPBOARD 'TARGETS)))
             (seq-some (lambda (type)
                         (and (symbolp type)
                              (string-match-p "^image/" (symbol-name type))))
                       (if (vectorp targets) (append targets nil) targets))))
      (call-interactively #'yank-media)
    (apply orig-fun args)))

;;;; DWIM / query functions

(defun eca-chat--find-typed-query (prefix)
  "Return the text typed after the last item after PREFIX (@ or #).
For example: `@foo @bar @baz` => `baz`. If nothing is typed, resteps an empty
string."
  (when (eca-chat--point-at-new-context-p)
    (save-excursion
      (goto-char (eca-chat--new-context-start-point))
      (end-of-line)))
  (save-excursion
    (let* ((start (line-beginning-position))
           (end (point))
           (last-prefix-pos (search-backward prefix start t)))
      (if last-prefix-pos
          (string-trim (buffer-substring-no-properties (+ last-prefix-pos (length prefix)) end))
        ""))))

(declare-function dired-get-marked-files "dired")
(declare-function treemacs-node-at-point "treemacs")
(declare-function treemacs-button-get "treemacs")

(defun eca-chat--get-contexts-dwim ()
  "Get contexts in a DWIM manner."
  (cond
   ((and (buffer-file-name)
         (use-region-p))
    (-let (((start . end) `(,(line-number-at-pos (region-beginning)) . ,(line-number-at-pos (region-end)))))
      (list
       (list :type "file"
             :path (buffer-file-name)
             :linesRange (list :start start :end end)))))

   ((derived-mode-p 'dired-mode)
    (--map (list :type (if (f-dir? it) "directory" "file")
                 :path it)
           (dired-get-marked-files)))

   ((derived-mode-p 'treemacs-mode)
    (when-let (path (-some-> (treemacs-node-at-point)
                      (treemacs-button-get :path)))
      (list
       (list :type (if (f-dir? path) "directory" "file")
             :path path))))

   ((buffer-file-name)
    (list
     (list :type "file" :path (buffer-file-name))))))

;;;; Completion functions

(defun eca-chat--completion-item-kind (item)
  "Return the kind for ITEM."
  (alist-get (plist-get item :type)
             eca-chat--kind->symbol
             nil
             nil
             #'string=))

(defun eca-chat--completion-item-label-kind (item-label)
  "Return the kind for ITEM-LABEL."
  (eca-chat--completion-item-kind (get-text-property 0 'eca-chat-completion-item item-label)))

(defun eca-chat--completion-item-company-box-icon (item-label)
  "Return the kind for ITEM-LABEL."
  (let ((symbol (eca-chat--completion-item-label-kind item-label)))
    (intern (capitalize (symbol-name symbol)))))

(defun eca-chat--completion-context-annotate (roots item-label)
  "Annonate ITEM-LABEL detail for ROOTS."
  (-let (((&plist :type type :path path :description description) (get-text-property 0 'eca-chat-completion-item item-label)))
    (pcase type
      ("file" (eca-chat--relativize-filename-for-workspace-root path roots 'hide-filename))
      ("directory" (eca-chat--relativize-filename-for-workspace-root path roots 'hide-filename))
      ("repoMap" "Summary view of workspaces files")
      ("cursor" "Current cursor file + position")
      ("mcpResource" description)
      (_ ""))))

(defun eca-chat--completion-file-annotate (roots item-label)
  "Annonate ITEM-LABEL detail for ROOTS."
  (-let (((&plist :path path) (get-text-property 0 'eca-chat-completion-item item-label)))
    (eca-chat--relativize-filename-for-workspace-root path roots 'hide-filename)))

(defun eca-chat--completion-prompts-annotate (item-label)
  "Annotate prompt ITEM-LABEL."
  (-let (((&plist :description description :arguments args)
          (get-text-property 0 'eca-chat-completion-item item-label)))
    (concat "(" (string-join (--map (plist-get it :name) args) ", ")
            ") "
            (when description
              (truncate-string-to-width description (* 100 eca-chat-window-width))))))

(defun eca-chat--completion-context-from-new-context-exit-function (item _status)
  "Add to context the selected ITEM."
  (eca-chat--add-context (get-text-property 0 'eca-chat-completion-item item))
  (end-of-line))

(defun eca-chat--completion-context-from-prompt-exit-function (item _status)
  "Add to context the selected ITEM.
Add text property to prompt text to match context."
  (let ((context (get-text-property 0 'eca-chat-completion-item item)))
    (let ((start-pos (save-excursion
                       (search-backward eca-chat-context-prefix (line-beginning-position) t)))
          (end-pos (point)))
      (delete-region start-pos end-pos)
      (eca-chat--insert (eca-chat--context->str context 'static))))
  (eca-chat--insert " "))

(defun eca-chat--completion-file-from-prompt-exit-function (item _status)
  "Add to files the selected ITEM."
  (let* ((file (get-text-property 0 'eca-chat-completion-item item))
         (start-pos (save-excursion
                      (search-backward eca-chat-filepath-prefix (line-beginning-position) t)))
         (end-pos (point)))
    (delete-region start-pos end-pos)
    (eca-chat--insert (eca-chat--filepath->str (plist-get file :path) nil)))
  (eca-chat--insert " "))

(defun eca-chat--completion-prompt-exit-function (item _status)
  "Finish prompt completion for ITEM."
  (-let* (((&plist :arguments arguments) (get-text-property 0 'eca-chat-completion-item item)))
    (when (> (length arguments) 0)
      (seq-doseq (arg arguments)
        (-let (((&plist :name name :description description :required required) arg))
          (eca-chat--insert " ")
          (let ((arg-text (read-string (format "Arg: %s\nDescription: %s\nValue%s: "
                                               name
                                               (or description "")
                                               (if required "" " (leave blank for default)")))))
            (if (and arg-text (string-match-p " " arg-text))
                (eca-chat--insert (format "\"%s\"" arg-text))
              (eca-chat--insert arg-text)))))
      (end-of-line))))

(defun eca-chat--context-to-completion (context)
  "Convert CONTEXT to a completion item."
  (let* ((ctx-type (plist-get context :type))
         (ctx-path (plist-get context :path))
         (raw-label (pcase ctx-type
                      ("file" (f-filename ctx-path))
                      ("directory" (f-filename ctx-path))
                      ("repoMap" "repoMap")
                      ("cursor" "cursor")
                      ("mcpResource" (concat (plist-get context :server) ":" (plist-get context :name)))
                      (_ (concat "Unknown - " ctx-type))))
         (face (pcase ctx-type
                 ("file" 'eca-chat-context-file-face)
                 ("directory" 'eca-chat-context-file-face)
                 ("repoMap" 'eca-chat-context-repo-map-face)
                 ("cursor" 'eca-chat-context-cursor-face)
                 ("mcpResource" 'eca-chat-context-mcp-resource-face)
                 (_ nil))))
    (propertize raw-label
                'eca-chat-completion-item context
                'face face)))

(defun eca-chat--file-to-completion (file)
  "Convert FILE to a completion item."
  (propertize (f-filename (plist-get file :path))
              'eca-chat-completion-item file
              'face 'eca-chat-context-file-face))

(defun eca-chat--command-to-completion (command)
  "Convert COMMAND to a completion item."
  (propertize (plist-get command :name)
              'eca-chat-completion-item command))

;;;; Eldoc function

(defun eca-chat-eldoc-function (cb &rest _ignored)
  "Eldoc function to show details of context and prompt in eldoc.
Calls CB with the resulting message."
  (cond
   ;; Task eldoc: show description and blocked-by
   ((when-let* ((task (get-text-property (point) 'eca-chat-task)))
      (let* ((subject (plist-get task :subject))
             (description (plist-get task :description))
             (blocked-by (append (plist-get task :blockedBy) nil))
             (blocked-subjects
              (when blocked-by
                (mapcar (lambda (id)
                          (if-let* ((tk (eca-chat--task-find-by-id id)))
                              (plist-get tk :subject)
                            (format "#%s" id)))
                        blocked-by)))
             (doc (concat
                   (propertize subject 'face 'bold)
                   (when description
                     (concat "\n" description))
                   (when blocked-subjects
                     (concat "\n"
                             (propertize "Blocked by: " 'face 'font-lock-keyword-face)
                             (string-join blocked-subjects ", "))))))
        (funcall cb doc)
        t)))
   ;; Context/filepath eldoc
   ((when-let ((item-type (get-text-property (point) 'eca-chat-item-type)))
      (when-let ((item-str (get-text-property (point) 'eca-chat-expanded-item-str)))
        (when-let ((face (get-text-property (point) 'font-lock-face)))
          (funcall cb (format "%s: %s"
                              (pcase item-type
                                ('context "Context")
                                ('filepath "Filepath"))
                              (propertize item-str 'face face)))
          t))))))

;;;; Completion-at-point function

(defun eca-chat-completion-at-point ()
  "Complete at point in the chat."
  (let* ((full-text (buffer-substring-no-properties (line-beginning-position) (point)))
         (type (cond
                ;; completing contexts
                ((eca-chat--point-at-new-context-p)
                 'contexts-from-new-context)

                ((when-let (last-word (car (last (string-split full-text "[\s]"))))
                   (string-match-p (concat "^" eca-chat-context-prefix) last-word))
                 'contexts-from-prompt)

                ((when-let (last-word (car (last (string-split full-text "[\s]"))))
                   (string-match-p (concat "^" eca-chat-filepath-prefix) last-word))
                 'files-from-prompt)

                ;; completing commands with `/`
                ((and (eca-chat--point-at-prompt-field-p)
                      (string-prefix-p "/" full-text))
                 'prompts)

                (t nil)))
         (bounds-start (pcase type
                         ('prompts (1+ (line-beginning-position)))
                         (_ (or
                             (cl-first (bounds-of-thing-at-point 'symbol))
                             (point)))))
         (candidates-fn (lambda ()
                          (eca-api-catch 'input
                              (eca-api-while-no-input
                                (pcase type
                                  ((or 'contexts-from-prompt
                                       'contexts-from-new-context)
                                   (let ((query (eca-chat--find-typed-query eca-chat-context-prefix)))
                                     (or (gethash query eca-chat--context-completion-cache)
                                         (-let* (((&plist :contexts contexts) (eca-api-request-while-no-input
                                                                               (eca-session)
                                                                               :method "chat/queryContext"
                                                                               :params (list :chatId eca-chat--id
                                                                                             :query query
                                                                                             :contexts (vconcat eca-chat--context))))
                                                 (items (-map #'eca-chat--context-to-completion contexts)))
                                           (clrhash eca-chat--context-completion-cache)
                                           (puthash query items eca-chat--context-completion-cache)
                                           items))))

                                  ('files-from-prompt
                                   (let ((query (eca-chat--find-typed-query eca-chat-filepath-prefix)))
                                     (or (gethash query eca-chat--file-completion-cache)
                                         (-let* (((&plist :files files) (eca-api-request-while-no-input
                                                                         (eca-session)
                                                                         :method "chat/queryFiles"
                                                                         :params (list :chatId eca-chat--id
                                                                                       :query query)))
                                                 (items (-map #'eca-chat--file-to-completion files)))
                                           (clrhash eca-chat--file-completion-cache)
                                           (puthash query items eca-chat--file-completion-cache)
                                           items))))

                                  ('prompts
                                   (let ((query (substring full-text 1)))
                                     (or (gethash query eca-chat--command-completion-cache)
                                         (-let* (((&plist :commands commands) (eca-api-request-while-no-input
                                                                               (eca-session)
                                                                               :method "chat/queryCommands"
                                                                               :params (list :chatId eca-chat--id
                                                                                             :query query)))
                                                 (items (-map #'eca-chat--command-to-completion commands)))
                                           (clrhash eca-chat--command-completion-cache)
                                           (puthash query items eca-chat--command-completion-cache)
                                           items))))

                                  (_ nil)))
                            (:interrupted nil)
                            (`,res res))))
         (exit-fn (pcase type
                    ('contexts-from-new-context #'eca-chat--completion-context-from-new-context-exit-function)
                    ('contexts-from-prompt #'eca-chat--completion-context-from-prompt-exit-function)
                    ('files-from-prompt #'eca-chat--completion-file-from-prompt-exit-function)
                    ('prompts #'eca-chat--completion-prompt-exit-function)
                    (_ nil)))
         (annotation-fn (pcase type
                          ((or 'contexts-from-prompt
                               'contexts-from-new-context) (-partial #'eca-chat--completion-context-annotate (eca--session-workspace-folders (eca-session))))
                          ('files-from-prompt (-partial #'eca-chat--completion-file-annotate (eca--session-workspace-folders (eca-session))))
                          ('prompts #'eca-chat--completion-prompts-annotate))))
    (list
     bounds-start
     (point)
     (lambda (probe pred action)
       (cond
        ((eq action 'metadata)
         '(metadata (category . eca-capf)
                    (display-sort-function . identity)
                    (cycle-sort-function . identity)))
        ((eq (car-safe action) 'boundaries) nil)
        (t
         (complete-with-action action (funcall candidates-fn) probe pred))))
     :company-kind #'eca-chat--completion-item-label-kind
     :company-require-match 'never
     :annotation-function annotation-fn
     :exit-function exit-fn)))

(provide 'eca-chat-context)
;;; eca-chat-context.el ends here
