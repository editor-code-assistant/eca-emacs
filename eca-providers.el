;;; eca-providers.el --- ECA provider/model management -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Provider and model management for ECA settings panel.
;;  Displays provider auth status, available models, and
;;  handles login/logout flows via the providers/* protocol.
;;
;;; Code:

(require 'compat)

(require 'eca-util)
(require 'eca-api)
(require 'eca-settings)

;; Faces

(defface eca-providers-authenticated-face
  '((t (:foreground "green" :weight bold)))
  "Face for authenticated provider indicator."
  :group 'eca)

(defface eca-providers-local-face
  '((t (:foreground "dodger blue" :weight bold)))
  "Face for local provider indicator."
  :group 'eca)

(defface eca-providers-unauthenticated-face
  '((t (:inherit shadow)))
  "Face for unauthenticated provider indicator."
  :group 'eca)

(defface eca-providers-button-face
  '((t (:inherit button)))
  "Face for action buttons."
  :group 'eca)

(defface eca-providers-button-logout-face
  '((t (:inherit warning :underline t)))
  "Face for logout button."
  :group 'eca)

(defface eca-providers-model-face
  '((t (:inherit font-lock-doc-face)))
  "Face for model names."
  :group 'eca)

(defface eca-providers-auth-info-face
  '((t (:inherit font-lock-doc-face :height 0.9)))
  "Face for auth info line."
  :group 'eca)

(defface eca-providers-section-face
  '((t (:inherit shadow)))
  "Face for section separators."
  :group 'eca)

;; Buffer-local state

(defvar-local eca-providers--expanded nil
  "Set of provider IDs that are expanded to show models.")

;; Keymap

(defvar eca-providers-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map eca-settings-mode-map)
    (define-key map (kbd "l") #'eca-providers--login-at-point)
    (define-key map (kbd "d") #'eca-providers--logout-at-point)
    (define-key map (kbd "RET") #'eca-providers--toggle-expand)
    (define-key map (kbd "TAB") #'eca-providers--toggle-expand)
    map)
  "Keymap for the Providers settings tab.")

;; Status helpers

(defun eca-providers--status-indicator (status)
  "Return a colored indicator string for auth STATUS."
  (pcase status
    ((or "authenticated" "expiring" "expired")
     (propertize "●" 'face 'eca-providers-authenticated-face))
    ("local"         (propertize "●" 'face 'eca-providers-local-face))
    ((or "unauthenticated" "not-running")
     (propertize "○" 'face 'eca-providers-unauthenticated-face))
    (_               (propertize "·" 'face 'shadow))))

(defun eca-providers--status-label (status source)
  "Return a label string for auth STATUS from SOURCE."
  (pcase status
    ((or "authenticated" "expiring" "expired")
     (if (member source '("config" "env"))
         (propertize "✓ configured" 'face 'eca-providers-authenticated-face)
       (propertize "✓ authenticated" 'face 'eca-providers-authenticated-face)))
    ("local"         (propertize "local" 'face 'eca-providers-local-face))
    ("not-running"   (propertize "not running" 'face 'eca-providers-unauthenticated-face))
    ("unauthenticated" (propertize "✗ not authenticated" 'face 'eca-providers-unauthenticated-face))
    (_               (propertize "not configured" 'face 'shadow))))

(defun eca-providers--auth-info-text (auth)
  "Build the auth info line text from AUTH plist."
  (let ((status (plist-get auth :status))
        (type (plist-get auth :type))
        (mode (plist-get auth :mode))
        (source (plist-get auth :source))
        (expires-at (plist-get auth :expiresAt))
        (env-var (plist-get auth :envVar)))
    (cond
     ((string= status "local") nil)
     ((string= status "unauthenticated") nil)
     (t
      (let ((parts '()))
        (when mode (push (capitalize mode) parts))
        (when (and type (not mode))
          (push (if (string= type "oauth") "OAuth" "API Key") parts))
        (cond
         ((and (string= source "env") env-var)
          (push (format "(env: %s)" env-var) parts))
         ((string= source "config") (push "(config)" parts))
         ((string= source "login") (push "(login)" parts)))
        (when expires-at
          (let ((diff (- expires-at (float-time))))
            (when (> diff 0)
              (cond
               ((<= diff 3600)
                (push (format "expires in %dm" (max 1 (/ (round diff) 60))) parts))
               ((<= diff 86400)
                (push (format "expires in %dh" (/ (round diff) 3600)) parts))
               (t
                (push (format "expires in %dd" (/ (round diff) 86400)) parts))))))
        (when (and (string= status "authenticated") (not expires-at))
          (push "active" parts))
        (string-join (nreverse parts) " · "))))))

(defun eca-providers--format-setting-value (value)
  "Format a settings VALUE for display.
Plists/alists become compact JSON-like strings, others use `format'."
  (cond
   ((and (listp value) (plistp value) value)
    (let ((pairs '()))
      (cl-loop for (k v) on value by #'cddr
               do (push (format "%s: %s"
                                (substring (symbol-name k) 1)
                                (eca-providers--format-setting-value v))
                        pairs))
      (concat "{" (string-join (nreverse pairs) ", ") "}")))
   ((vectorp value)
    (concat "[" (string-join (mapcar #'eca-providers--format-setting-value
                                     (append value nil))
                             ", ")
            "]"))
   ((stringp value) value)
   ((eq value t) "true")
   ((null value) "false")
   (t (format "%s" value))))

(defun eca-providers--render-settings (settings indent)
  "Render SETTINGS plist as key-val lines with INDENT prefix."
  (when settings
    (cl-loop for (k v) on settings by #'cddr
             do (let ((key (substring (symbol-name k) 1))
                      (val (eca-providers--format-setting-value v)))
                  (insert indent)
                  (insert (propertize key 'face 'font-lock-keyword-face))
                  (insert (propertize ": " 'face 'shadow))
                  (insert (propertize val 'face 'eca-providers-auth-info-face))
                  (insert "\n")))))

;; Provider sorting

(defun eca-providers--sort-key (provider)
  "Return a sort key for PROVIDER (lower = higher priority)."
  (let* ((auth (plist-get provider :auth))
         (status (plist-get auth :status))
         (configured (plist-get provider :configured)))
    (cond
     ((member status '("authenticated" "expiring" "expired")) 0)
     ((string= status "local") 1)
     ((and configured (member status '("unauthenticated" "not-running"))) 2)
     (t 3))))

(defun eca-providers--has-oauth-login-p (login-methods)
  "Return non-nil if LOGIN-METHODS includes an OAuth or device flow.
When all methods are just API key entry, returns nil."
  (seq-some (lambda (m)
              (not (member (plist-get m :key) '("manual" "api-key"))))
            login-methods))

;; Rendering

(defun eca-providers--render-models (models)
  "Render expanded model list for MODELS vector."
  (seq-doseq (model models)
    (let ((id (plist-get model :id))
          (settings (plist-get model :settings)))
      (insert "   │ ")
      (insert (propertize id 'face 'eca-providers-model-face))
      (when settings
        (let ((parts '()))
          (cl-loop for (k v) on settings by #'cddr
                   do (push (concat (propertize (substring (symbol-name k) 1)
                                                'face 'font-lock-keyword-face)
                                    (propertize ": " 'face 'shadow)
                                    (propertize (eca-providers--format-setting-value v)
                                                'face 'eca-providers-auth-info-face))
                            parts))
          (insert "  ")
          (insert (string-join (nreverse parts) "  "))))
      (insert "\n"))))

(defun eca-providers--render-provider (provider session keymap)
  "Render a single PROVIDER card for SESSION using KEYMAP for buttons."
  (let* ((id (plist-get provider :id))
         (label (or (plist-get provider :label) id))
         (configured (plist-get provider :configured))
         (auth (plist-get provider :auth))
         (status (plist-get auth :status))
         (login-methods (plist-get (plist-get provider :login) :methods))
         (models (plist-get provider :models))
         (model-count (length models))
         (expanded (and eca-providers--expanded
                        (member id eca-providers--expanded)))
         (has-login (and login-methods (> (length login-methods) 0)))
         (is-authed (member status '("authenticated" "expiring" "expired" "local"))))
    ;; Line 1: indicator + name + status label
    (let ((start (point)))
      (insert " " (eca-providers--status-indicator status) " ")
      (insert (propertize label 'face 'bold))
      (let ((status-label (eca-providers--status-label status (plist-get auth :source))))
        (insert (make-string (max 1 (- 35 (length label))) ?\s))
        (insert status-label))
      (insert "\n")
      ;; Line 2: auth info (if any)
      (when-let* ((info (eca-providers--auth-info-text auth)))
        (insert "   " (propertize info 'face 'eca-providers-auth-info-face) "\n"))
      ;; Provider settings
      (eca-providers--render-settings (plist-get provider :settings) "   ")
      ;; Line 3: models count + actions
      (when configured
        (let* ((toggle-fn (lambda (&rest _)
                            (interactive)
                            (eca-providers--toggle-expand)))
               (expand-km (let ((km (make-composed-keymap (make-sparse-keymap) keymap)))
                            (define-key km (kbd "RET") toggle-fn)
                            (define-key km (kbd "<return>") toggle-fn)
                            (define-key km (kbd "TAB") toggle-fn)
                            (define-key km (kbd "<tab>") toggle-fn)
                            (when eca-buttons-allow-mouse
                              (define-key km (kbd "<mouse-1>") toggle-fn))
                            km))
               (models-text (concat "   "
                                    (propertize (if expanded "▾" "▸") 'face 'bold)
                                    " "
                                    (propertize (format "%d model%s" model-count (if (= model-count 1) "" "s"))
                                                'face 'font-lock-doc-face))))
          (insert (propertize models-text
                              'keymap expand-km
                              'pointer 'hand
                              'help-echo "RET / TAB / mouse-1: expand/collapse")))
        ;; Action buttons
        (let ((btn-col (max (+ (current-column) 2) 40))
              (source (plist-get auth :source)))
          (insert (make-string (max 1 (- btn-col (current-column))) ?\s))
          (cond
           ((and is-authed (string= source "login"))
            (insert (eca-buttonize
                     keymap
                     (propertize "Logout"
                                 'font-lock-face 'eca-providers-button-logout-face)
                     (lambda ()
                       (when (y-or-n-p (format "Logout from %s? " label))
                         (eca-api-request-async session
                           :method "providers/logout"
                           :params (list :provider id)
                           :success-callback (lambda (_) (eca-info "Logged out of %s" label))
                           :error-callback (lambda (err) (eca-warn "Logout failed: %s" err))))))))
           ((and has-login (not is-authed))
            (let ((btn-label (if (eca-providers--has-oauth-login-p login-methods) "Login" "Add Key")))
              (insert (eca-buttonize
                       keymap
                       (propertize btn-label
                                   'font-lock-face 'eca-providers-button-face)
                       (lambda () (eca-providers--do-login session id nil))))))))
        (insert "\n")
        ;; Expanded models
        (when (and expanded (> model-count 0))
          (eca-providers--render-models models)))
      ;; Unconfigured: show login/add-key button
      (when (and (not configured) has-login)
        (let ((btn-label (if (eca-providers--has-oauth-login-p login-methods) "Login" "Add Key")))
          (insert "   "
                  (eca-buttonize
                   keymap
                   (propertize btn-label
                               'font-lock-face 'eca-providers-button-face)
                   (lambda () (eca-providers--do-login session id nil)))
                  "\n")))
      ;; Tag the whole card with provider id
      (put-text-property start (point) 'eca-provider-id id))
    (insert "\n")))

(defun eca-providers--render (session buffer)
  "Render provider list for SESSION into BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (keymap (current-local-map))
            (providers (eca--session-providers session)))
        (erase-buffer)
        (insert "\n")
        (insert (propertize "Providers / Models" 'font-lock-face 'helpful-heading))
        (insert "\n")
        (insert (propertize "For more details check " 'face 'shadow))
        (insert-text-button "https://eca.dev/config/models/"
                            'face 'link
                            'action (lambda (_) (browse-url "https://eca.dev/config/models/"))
                            'help-echo "Open in browser")
        (insert "\n\n")
        (if (not providers)
            (insert (propertize "  Loading providers..." 'face 'shadow))
          (let* ((sorted (-sort (lambda (a b)
                                  (< (eca-providers--sort-key a)
                                     (eca-providers--sort-key b)))
                                providers))
                 (configured (-filter (lambda (p) (plist-get p :configured)) sorted))
                 (unconfigured (-filter (lambda (p) (not (plist-get p :configured))) sorted)))
            (seq-doseq (provider configured)
              (eca-providers--render-provider provider session keymap))
            (when unconfigured
              (insert (propertize " ── not configured ─────────────────────\n\n"
                                  'face 'eca-providers-section-face))
              (seq-doseq (provider unconfigured)
                (eca-providers--render-provider provider session keymap)))))
        (insert "\n")))))

;; Login flow

(defun eca-providers--do-login (session provider-id method)
  "Start login for PROVIDER-ID with optional METHOD via SESSION."
  (eca-api-request-async session
    :method "providers/login"
    :params (if method
                (list :provider provider-id :method method)
              (list :provider provider-id))
    :success-callback
    (lambda (result)
      (eca-providers--handle-login-response session provider-id result))
    :error-callback
    (lambda (err)
      (eca-warn "Login failed: %s" err))))

(defun eca-providers--collect-fields (fields)
  "Collect input for FIELDS via minibuffer prompts. Returns a plist."
  (let ((data '()))
    (seq-doseq (field fields)
      (let* ((key (plist-get field :key))
             (label (plist-get field :label))
             (type (plist-get field :type))
             (value (if (string= type "secret")
                        (read-passwd (format "%s: " label))
                      (read-string (format "%s: " label)))))
        (setq data (plist-put data (intern (concat ":" key)) value))))
    data))

(defun eca-providers--handle-login-response (session provider-id result)
  "Handle login RESULT for PROVIDER-ID from SESSION."
  (pcase (plist-get result :action)
    ("choose-method"
     (let* ((methods (append (plist-get result :methods) nil))
            (choices (mapcar (lambda (m)
                              (cons (plist-get m :label)
                                    (plist-get m :key)))
                            methods))
            (selected (completing-read "Login method: " choices nil t)))
       (when-let* ((method (cdr (assoc selected choices))))
         (eca-providers--do-login session provider-id method))))
    ("input"
     (let ((data (eca-providers--collect-fields
                  (append (plist-get result :fields) nil))))
       (eca-api-request-async session
         :method "providers/loginInput"
         :params (list :provider provider-id :data data)
         :success-callback (lambda (_) (eca-info "Login complete"))
         :error-callback (lambda (err) (eca-warn "Login failed: %s" err)))))
    ("authorize"
     (let ((url (plist-get result :url))
           (fields (plist-get result :fields))
           (msg (plist-get result :message)))
       (browse-url url)
       (if fields
           ;; Browser auth + code paste (e.g. Anthropic)
           (let ((data (eca-providers--collect-fields (append fields nil))))
             (eca-api-request-async session
               :method "providers/loginInput"
               :params (list :provider provider-id :data data)
               :success-callback (lambda (_) (eca-info "Login complete"))
               :error-callback (lambda (err) (eca-warn "Login failed: %s" err))))
         ;; Auto-callback (e.g. OpenAI) — wait for providers/updated
         (eca-info "%s" (or msg "Completing authentication in browser...")))))
    ("device-code"
     (let ((url (plist-get result :url))
           (code (plist-get result :code))
           (msg (plist-get result :message)))
       (kill-new code)
       (browse-url url)
       (eca-info "Code %s copied to clipboard. %s" code (or msg ""))))
    ("done"
     (eca-info "Login complete"))))

;; Point helpers

(defun eca-providers--provider-id-at-point ()
  "Return the provider ID at or nearest before point."
  (or (get-text-property (point) 'eca-provider-id)
      (save-excursion
        (when-let* ((match (text-property-search-backward 'eca-provider-id nil (lambda (_ v) v))))
          (get-text-property (prop-match-beginning match) 'eca-provider-id)))))

(defun eca-providers--goto-provider (provider-id)
  "Move point to the first line of PROVIDER-ID card."
  (goto-char (point-min))
  (when-let* ((match (text-property-search-forward 'eca-provider-id provider-id #'equal)))
    (goto-char (prop-match-beginning match))))

;; Interactive commands

(defun eca-providers--login-at-point ()
  "Start login flow for the provider at point."
  (interactive)
  (if-let* ((session (ignore-errors (eca-session)))
            (provider-id (eca-providers--provider-id-at-point)))
      (eca-providers--do-login session provider-id nil)
    (user-error "No provider at point")))

(defun eca-providers--logout-at-point ()
  "Logout from the provider at point."
  (interactive)
  (if-let* ((session (ignore-errors (eca-session)))
            (provider-id (eca-providers--provider-id-at-point)))
      (when (y-or-n-p (format "Logout from %s? " provider-id))
        (eca-api-request-async session
          :method "providers/logout"
          :params (list :provider provider-id)
          :success-callback (lambda (_) (eca-info "Logged out of %s" provider-id))
          :error-callback (lambda (err) (eca-warn "Logout failed: %s" err))))
    (user-error "No provider at point")))

(defun eca-providers--toggle-expand ()
  "Toggle model list expansion for the provider at point."
  (interactive)
  (when-let* ((provider-id (eca-providers--provider-id-at-point))
              (session (ignore-errors (eca-session))))
    (let* ((card-start (previous-single-property-change
                        (1+ (point)) 'eca-provider-id nil (point-min)))
           (line-offset (count-lines card-start (point)))
           (col (current-column)))
      (if (member provider-id eca-providers--expanded)
          (setq eca-providers--expanded (delete provider-id eca-providers--expanded))
        (push provider-id eca-providers--expanded))
      (eca-providers--render session (current-buffer))
      (eca-providers--goto-provider provider-id)
      (forward-line line-offset)
      (move-to-column col))))

;; Notification handler

(defun eca-providers--handle-provider-updated (session provider-status)
  "Update PROVIDER-STATUS in SESSION and refresh the tab."
  (let* ((id (plist-get provider-status :id))
         (providers (eca--session-providers session))
         (updated (if providers
                      (cons provider-status
                            (seq-remove (lambda (p)
                                          (string= (plist-get p :id) id))
                                        providers))
                    (list provider-status))))
    (setf (eca--session-providers session) updated)
    (eca-settings-refresh-tab "providers" session)))

;; Data loading

(defun eca-providers--fetch-and-render (session buffer)
  "Fetch provider list from SESSION and render into BUFFER."
  (eca-api-request-async session
    :method "providers/list"
    :params nil
    :success-callback
    (lambda (result)
      (setf (eca--session-providers session)
            (append (plist-get result :providers) nil))
      (when (buffer-live-p buffer)
        (eca-providers--render session buffer)))
    :error-callback
    (lambda (err)
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "\n")
            (insert (propertize "Failed to load providers" 'face 'error))
            (insert (format "\n\n  %s" err))))))))

;; Settings tab

(defun eca-providers--create-settings-buffer (session)
  "Create the Providers settings tab buffer for SESSION."
  (let ((buf (eca-settings--create-buffer "providers" session)))
    (with-current-buffer buf
      (eca-settings-mode)
      (use-local-map eca-providers-mode-map)
      (visual-line-mode)
      (eca-settings--setup-tab-line "providers")
      (eca-providers--render session buf)
      (eca-providers--fetch-and-render session buf))
    buf))

;;;###autoload
(defun eca-providers ()
  "Open the providers/models settings tab."
  (interactive)
  (eca-settings "providers"))

(eca-settings-register-tab
 "providers" "🔑 Providers"
 #'eca-providers--create-settings-buffer
 #'eca-providers--render)

(provide 'eca-providers)
;;; eca-providers.el ends here
