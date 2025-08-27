;;; eca-process.el --- ECA (Editor Code Assistant) process -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Eric Dallo
;;
;; SPDX-License-Identifier: Apache-2.0
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  The ECA (Editor Code Assistant) process.
;;
;;; Code:

(require 's)
(require 'f)

(require 'eca-util)
(require 'eca-api)

(defcustom eca-custom-command nil
  "The eca server command.
If not provided, download and start eca automatically."
  :group 'eca
  :risky t
  :type '(repeat string))

(defcustom eca-extra-args '()
  "The extra args to pass to eca server command."
  :group 'eca
  :risky t
  :type '(repeat string))

(defcustom eca-server-download-url nil
  "The custom URL to download eca server."
  :group 'eca
  :type 'string)

(defcustom eca-server-install-path
  (f-join (expand-file-name
           (locate-user-emacs-file "eca"))
          (if (eq system-type 'windows-nt)
              "eca.exe"
            "eca"))
  "Directory in which eca will be downloaded."
  :risky t
  :type 'directory
  :group 'eca)

(defcustom eca-server-version-file-path
  (f-join (expand-file-name
           (locate-user-emacs-file "eca"))
          "eca-version")
  "File in which eca version will be."
  :risky t
  :type 'string
  :group 'eca)

(defconst eca-ext-pwsh-script "pwsh -noprofile -noninteractive \
-nologo -ex bypass -c Expand-Archive -Path '%s' -DestinationPath '%s'"
  "Pwsh script to unzip file.")

(defconst eca-ext-powershell-script "powershell -noprofile -noninteractive \
-nologo -ex bypass -command Expand-Archive -path '%s' -dest '%s'"
  "Powershell script to unzip file.")

(defconst eca-ext-unzip-script "bash -c 'mkdir -p %2$s && unzip -qq -o %1$s -d %2$s'"
  "Unzip script to unzip file.")

(defcustom eca-unzip-script (lambda ()
                              (cond ((and (eq system-type 'windows-nt)
                                          (executable-find "pwsh"))
                                     eca-ext-pwsh-script)
                                    ((and (eq system-type 'windows-nt)
                                          (executable-find "powershell"))
                                     eca-ext-powershell-script)
                                    ((executable-find "unzip") eca-ext-unzip-script)
                                    ((executable-find "pwsh") eca-ext-pwsh-script)
                                    (t nil)))
  "The script to unzip downloaded eca server."
  :group 'eca
  :type 'string)

(defun eca-process--buffer-name (session)
  "Return the process buffer name for SESSION."
  (format  "<eca:%s>" (eca--session-id session)))

(defun eca-process--stderr-buffer-name (session)
  "Return the stderr buffer name for SESSION."
  (format  "<eca:stderr:%s>" (eca--session-id session)))

(defvar eca-process--latest-server-version nil)

(defun eca--download-file (url path)
  "Download a file from URL to PATH shelling out to system.
Workaround for `url-copy-file` that has issues with macos async threads.
https://github.com/emacs-lsp/lsp-mode/issues/4746#issuecomment-2957183423"
  (let ((curl-cmd (or (executable-find "curl")
                      (executable-find "curl.exe"))))
    (unless curl-cmd
      (error "Curl not found. Please install curl or customize eca-custom-command"))
    (let ((exit-code (shell-command (format "%s -L -s -S -f -o %s %s"
                                            (shell-quote-argument curl-cmd)
                                            (shell-quote-argument path)
                                            (shell-quote-argument url)))))
      (unless (= exit-code 0)
        (error "Curl failed with exit code %d" exit-code)))))

(defun eca--download-string (url)
  "Download content from URL as a string, shelling out to curl."
  (let ((curl-cmd (or (executable-find "curl")
                      (executable-find "curl.exe"))))
    (unless curl-cmd
      (error "Curl not found. Please install curl or customize eca-custom-command"))
    (let ((output (shell-command-to-string
                   (format "%s -L -s -S -f %s"
                           (shell-quote-argument curl-cmd)
                           (shell-quote-argument url)))))
      (when (s-blank? output)
        (error "Curl failed to download from %s" url))
      output)))

(defun eca-process--get-latest-server-version ()
  "Return the latest server version."
  (or eca-process--latest-server-version
      (condition-case err
          (let* ((releases-url "https://api.github.com/repos/editor-code-assistant/eca/releases")
                 (json-string (eca--download-string releases-url)))
            (with-temp-buffer
              (insert json-string)
              (goto-char (point-min))
              (setq eca-process--latest-server-version
                    (plist-get (elt (eca-api--json-read-buffer) 0) :tag_name)))
            eca-process--latest-server-version)
        (error
         (eca-warn "Failed to get latest server version: %s" err)
         nil))))

(defun eca-process--get-current-server-version ()
  "Return the current version of installed server if available."
  (when (f-exists? eca-server-version-file-path)
    (f-read eca-server-version-file-path)))

(defun eca-process--download-and-store-path ()
  "Return the path of the download and store."
  (let* ((store-path eca-server-install-path)
         (download-path (concat store-path ".zip")))
    `(,download-path . ,store-path)))

(defun eca-process--uninstall-server ()
  "Remove downloaded server."
  (-let (((download-path . store-path) (eca-process--download-and-store-path)))
    (when (f-exists? download-path) (f-delete download-path))
    (when (f-exists? store-path) (f-delete store-path))))

(defun eca-process--download-url (version)
  "Return the server download url for VERSION."
  (or eca-server-download-url
      (format "https://github.com/editor-code-assistant/eca/releases/download/%s/eca-native-%s.zip"
              version
              (let ((arch (car (split-string system-configuration "-"))))
                (pcase system-type
                  ('gnu/linux (cond
                               ((string= "x86_64" arch) "static-linux-amd64")
                               (t (concat "linux-" arch))))
                  ('darwin (concat "macos-"
                                   (cond
                                    ((string= "x86_64" arch) "amd64")
                                    (t arch))))
                  ('windows-nt "windows-amd64"))))))

(defun eca-process--download-server (on-downloaded version)
  "Download eca server of VERSION calling ON-DOWNLOADED when success."
  (-let ((url (eca-process--download-url version))
         ((download-path . store-path) (eca-process--download-and-store-path)))
    (make-thread
     (lambda ()
       (condition-case err
           (progn
             (when (f-exists? download-path) (f-delete download-path))
             (when (f-exists? store-path) (f-delete store-path))
             (when (f-exists? eca-server-version-file-path) (f-delete eca-server-version-file-path))
             (mkdir (f-parent download-path) t)
             (eca-info "Downloading eca server from %s to %s..."  url download-path)
             (eca--download-file url download-path)
             (eca-info "Downloaded eca, unzipping it...")
             (unless eca-unzip-script
               (error "Unable to find `unzip' or `powershell' on the path, please customize `eca-unzip-script'"))
             (shell-command (format (funcall eca-unzip-script) download-path (f-parent store-path)))
             (f-write-text version 'utf-8 eca-server-version-file-path)
             (set-file-modes store-path #o0700))
         (error (eca-error "Could not download eca server %s" err)))
       (eca-info "Installed eca successfully!")
       (funcall on-downloaded)))))

(defun eca-process--server-command ()
  "Return the command to start server."
  (let ((system-command (executable-find "eca")))
    (cond
     (eca-custom-command (list :decision 'custom
                               :command eca-custom-command))

     (system-command
      (list :decision 'system
            :command (list system-command "server")))

     ((and (not (f-exists? eca-server-install-path))
           (not (eca-process--get-latest-server-version)))
      (list :decision 'error-download
            :message "Could not fetch latest version of eca. Please check your internet connection and try again. You can also download eca manually and set the path via eca-custom-command variable"))

     ((and (f-exists? eca-server-install-path)
           (string= (eca-process--get-latest-server-version)
                    (eca-process--get-current-server-version)))
      (list :decision 'already-installed
            :command (list eca-server-install-path "server")))

     (t (list :decision 'download
              :latest-version (eca-process--get-latest-server-version)
              :command (list eca-server-install-path "server"))))))

(defun eca-process--parse-header (s)
  "Parse string S as a ECA (KEY . VAL) header."
  (let ((pos (string-match "\:" s))
        key val)
    (unless pos
      (signal 'eca-invalid-header-name (list s)))
    (setq key (substring s 0 pos)
          val (s-trim-left (substring s (+ 1 pos))))
    (when (equal key "Content-Length")
      (cl-assert (cl-loop for c across val
                          when (or (> c ?9) (< c ?0)) return nil
                          finally return t)
                 nil (format "Invalid Content-Length value: %s" val)))
    (cons key val)))

(defun eca-process--filter (handle-msg _proc raw-output)
  "Process filter to parse eca's stdout RAW-OUTPUT delivering to HANDLE-MSG."
  (let ((body-received 0)
        leftovers body-length body chunk)
    (setf chunk (if (s-blank? leftovers)
                    (encode-coding-string raw-output 'utf-8-unix t)
                  (concat leftovers (encode-coding-string raw-output 'utf-8-unix t))))
    (let (messages)
      (while (not (s-blank? chunk))
        (if (not body-length)
            ;; Read headers
            (if-let* ((body-sep-pos (string-match-p "\r\n\r\n" chunk)))
                ;; We've got all the headers, handle them all at once:
                (setf body-length (let* ((headers (mapcar #'eca-process--parse-header
                                                          (split-string
                                                           (substring-no-properties chunk
                                                                                    (or (string-match-p "Content-Length" chunk)
                                                                                        (error "Unable to find Content-Length header"))
                                                                                    body-sep-pos)
                                                           "\r\n")))
                                         (content-length (cdr (assoc "Content-Length" headers))))
                                    (if content-length
                                        (string-to-number content-length)
                                      ;; This usually means either the server or our parser is
                                      ;; screwed up with a previous Content-Length
                                      (error "No Content-Length header")))
                      body-received 0
                      leftovers nil
                      chunk (substring-no-properties chunk (+ body-sep-pos 4)))

              ;; Haven't found the end of the headers yet. Save everything
              ;; for when the next chunk arrives and await further input.
              (setf leftovers chunk
                    chunk nil))
          (let* ((chunk-length (string-bytes chunk))
                 (left-to-receive (- body-length body-received))
                 (this-body (if (< left-to-receive chunk-length)
                                (prog1 (substring-no-properties chunk 0 left-to-receive)
                                  (setf chunk (substring-no-properties chunk left-to-receive)))
                              (prog1 chunk
                                (setf chunk nil))))
                 (body-bytes (string-bytes this-body)))
            (push this-body body)
            (setf body-received (+ body-received body-bytes))
            (when (>= chunk-length left-to-receive)
              (condition-case err
                  (with-temp-buffer
                    (apply #'insert
                           (nreverse
                            (prog1 body
                              (setf leftovers nil
                                    body-length nil
                                    body-received nil
                                    body nil))))
                    (decode-coding-region (point-min)
                                          (point-max)
                                          'utf-8)
                    (goto-char (point-min))
                    (push (eca-api--json-read-buffer) messages))

                (error
                 (eca-warn "Failed to parse the following chunk:\n'''\n%s\n'''\nwith message %s"
                           (concat leftovers raw-output)
                           err)))))))
      (mapc handle-msg
            (nreverse messages)))))

;; Public

(defun eca-process-start (session on-start handle-msg)
  "Start the eca process for SESSION calling ON-START after.
Call HANDLE-MSG for new msgs processed."
  (unless (process-live-p (eca--session-process session))
    (-let* (((result &as &plist :decision decision :command command) (eca-process--server-command))
            (start-process-fn (lambda ()
                                (let ((command (append command eca-extra-args)))
                                  (eca-info "Starting process '%s'" (string-join command " "))
                                  (setf (eca--session-process session)
                                        (make-process
                                         :coding 'no-conversion
                                         :connection-type 'pipe
                                         :name "eca"
                                         :command command
                                         :buffer (eca-process--buffer-name session)
                                         :stderr (get-buffer-create (eca-process--stderr-buffer-name session))
                                         :filter (-partial #'eca-process--filter handle-msg)
                                         :sentinel (lambda (process exit-str)
                                                     (unless (process-live-p process)
                                                       (eca-delete-session session)
                                                       (eca-info "process has exited (%s)" (s-trim exit-str))))
                                         :file-handler t
                                         :noquery t)))
                                (funcall on-start))))
      (pcase decision
        ('custom (funcall start-process-fn))

        ('system (funcall start-process-fn))

        ('error-download (user-error (eca-error (plist-get result :message))))

        ('already-installed (funcall start-process-fn))

        ('download (eca-process--download-server (lambda ()
                                                   (funcall start-process-fn))
                                                 (plist-get result :latest-version)))))))

(defun eca-process-running-p (session)
  "Return non nil if eca process for SESSION is running."
  (and session
       (process-live-p (eca--session-process session))))

(defun eca-process-stop (session)
  "Stop the eca process for SESSION if running."
  (when session
    (kill-process (eca--session-process session))
    (kill-buffer (eca-process--buffer-name session))
    ;; Rename stderr buffer to closed and clean up older closed ones
    (let ((stderr-buffer (get-buffer (eca-process--stderr-buffer-name session))))
      (when stderr-buffer
        (with-current-buffer stderr-buffer
          (rename-buffer (concat (buffer-name) ":closed") t)
          (setq-local mode-line-format '("*Closed session*"))
          (when-let ((win (get-buffer-window (current-buffer))))
            (quit-window nil win))
          ;; Keep only the most recently closed stderr buffer; kill older ones.
          (let ((current (current-buffer)))
            (dolist (b (buffer-list))
              (when (and (not (eq b current))
                         (or
                          (string-match-p "^<eca:stderr:.*>:closed$" (buffer-name b))
                          (string-match-p "^<eca:stderr:.*>$" (buffer-name b))))
                (kill-buffer b)))))))))

(defun eca-process-show-stderr (session)
  "Open the eca process stderr buffer for SESSION if running."
  (with-current-buffer (eca-process--stderr-buffer-name session)
    (if (window-live-p (get-buffer-window (buffer-name)))
        (select-window (get-buffer-window (buffer-name)))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun eca-show-stderr ()
  "Open the eca process stderr buffer if running."
  (interactive)
  (eca-process-show-stderr (eca-session)))

;;;###autoload
(defun eca-install-server ()
  "Force download the latest eca server."
  (interactive)
  (eca-process--download-server (lambda ())
                                (eca-process--get-latest-server-version)))

;;;###autoload
(defun eca-uninstall-server ()
  "Remove downloaded eca server if present."
  (interactive)
  (eca-process--uninstall-server)
  (eca-info "Server uninstalled!"))

(provide 'eca-process)
;;; eca-process.el ends here
